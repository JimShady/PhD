DROP VIEW IF EXISTS tube_ext;

--CREATE underground_stations table

DROP TABLE IF EXISTS underground_stations;

CREATE TABLE underground_stations(
name VARCHAR,
description VARCHAR,
x INTEGER,
y INTEGER);

-- Get the data from the CSV file
COPY underground_stations from '/home/james/mounts/James/PhD/2 - Routing/Tube/Raw Data/Underground_Stations_with_missing.csv' DELIMITERS ',' HEADER CSV;

-- Add an ID column and make it the primary key
ALTER TABLE underground_stations
ADD COLUMN id SERIAL PRIMARY KEY;

-- Add a Geom column

SELECT AddGeometryColumn ('underground_stations','the_geom',27700,'POINT',2);

-- Populate the Geom column from the easting and northing attributes

UPDATE underground_stations
SET the_geom = st_setsrid(st_makepoint("x", "y"),27700)
WHERE x is not null;

-- Now create an underground_routes table

DROP TABLE IF EXISTS underground_routes;

CREATE TABLE underground_routes(
shortname VARCHAR,
longname VARCHAR,
line VARCHAR,
in_order INTEGER,
section INTEGER,
ground_level numeric,
northbound numeric,
southbound numeric,
eastbound numeric,
westbound numeric,
inputted text
);

-- Get the data from the CSV file
-- Y:\James\PhD\2 - Routing\Tube\Raw Data
COPY underground_routes from '/home/james/mounts/James/PhD/2 - Routing/Tube/Raw Data/Lines_and_segments_created_with_depths.csv' DELIMITERS ',' CSV HEADER;

-- Add an ID column and make it the primary key
ALTER TABLE underground_routes
ADD COLUMN id SERIAL PRIMARY KEY;

-- Add a Geom column

SELECT AddGeometryColumn ('underground_routes','the_geom',27700,'POINT',2);

-- Join the geom of the points and the order and routes together into the underground_routes table.

UPDATE underground_routes
SET the_geom = underground_stations.the_geom
FROM underground_stations
WHERE underground_routes.longname = underground_stations.name;

-- Create a table with the manual coordinates of the tube stations that don't come automatically from TFL

DROP TABLE IF EXISTS manual_stations;

CREATE TABLE manual_stations(
id SERIAL PRIMARY KEY,
shortname VARCHAR,
longname VARCHAR
);

-- ADD the geometry columns to this table

SELECT AddGeometryColumn ('manual_stations','the_geom',27700,'POINT',2);

-- ADD THE TABLE MANUALLY TO THIS TABLE

INSERT INTO manual_stations (shortname, longname, the_geom)
VALUES
('Wood Lane', 'Wood Lane Station',  ST_Transform(st_geomfromtext('POINT(-0.2242 51.5098)', 4326), 27700)),
('Heathrow Terminal 5', 'Heathrow Terminal 5 Station',  ST_Transform(st_geomfromtext('POINT(-0.488 51.4723)', 4326), 27700)),
('Langdon Park', 'Langdon Park Station', ST_Transform(st_geomfromtext('POINT(-0.014 51.515)', 4326), 27700)),
('Star Lane', 'Star Lane Station', ST_Transform(st_geomfromtext('POINT(0.0042 51.5207)', 4326), 27700)),
('Abbey Road', 'Abbey Road Station', ST_Transform(st_geomfromtext('POINT(0.004 51.532)', 4326), 27700)),
('Stratford High Street', 'Stratford High Street Station', ST_Transform(st_geomfromtext('POINT(-0.0006 51.5379 )', 4326), 27700)),
('Stratford International', 'Stratford International Station', ST_Transform(st_geomfromtext('POINT(-0.0086 51.5448)', 4326), 27700)),
('West Silvertown', 'West Silvertown Station', ST_Transform(st_geomfromtext('POINT(0.0225 51.502778)', 4326), 27700)),
('Pontoon Dock', 'Pontoon Dock Station', ST_Transform(st_geomfromtext('POINT(0.031944 51.502222)', 4326), 27700)),
('London City Airport', 'London City Airport Station', ST_Transform(st_geomfromtext('POINT(0.048889 51.503611)', 4326), 27700)),
('King George V', 'King George V Station', ST_Transform(st_geomfromtext('POINT(0.062778 51.501972)', 4326), 27700)),
('Woolwich Arsenal', 'Woolwich Arsenal Station', ST_Transform(st_geomfromtext('POINT(0.069 51.49)', 4326), 27700))
;

-- Take the missing geoms from the manual_stations table and move them into the main underground_routes table

UPDATE underground_routes
SET the_geom = manual_stations.the_geom
FROM manual_stations
WHERE underground_routes.longname = manual_stations.longname;

-- STAGE TWO: MAKE THE LINES/ROUTES
-- Need start and end points for each of the lines.

DROP TABLE IF EXISTS temp;

CREATE TABLE	temp AS (
SELECT 		underground_routes.line,
		underground_routes.section,
		underground_routes.shortname AS start_station,
		underground_routes.in_order AS start_node,
		underground_routes.the_geom AS start_node_geom,
				a.shortname AS end_station,
		a.in_order AS end_node,
		a.the_geom AS end_node_geom
FROM 		underground_routes a
INNER JOIN 	underground_routes ON a.section = underground_routes.section
WHERE 		underground_routes.line = a.line
AND 		underground_routes.in_order <> a.in_order
ORDER BY 	underground_routes.line, section, start_node, end_node
);

--SELECT THE ABOVE TABLE AND MAKE A LINE BETWEEN EACH SET OF POINTS

DROP TABLE IF EXISTS underground_routes_processed CASCADE;

CREATE TABLE underground_routes_processed AS (
SELECT 	*,
	st_makeline(start_node_geom, end_node_geom) AS edge
FROM 	temp
WHERE 	temp.end_node - temp.start_node = '1'
OR 	temp.end_node - temp.start_node = '-1'
);

-- Now start to make the routing network

CREATE VIEW tube_ext AS
SELECT *, st_startpoint(edge), st_endpoint(edge)
FROM underground_routes_processed;

-- Create a table of nodes AKA change points/junctions. Remove duplicates.

DROP TABLE IF EXISTS tube_node;

CREATE TABLE tube_node AS 
   SELECT row_number() OVER (ORDER BY foo.p)::integer AS id, 
          foo.p AS the_geom
   FROM (         
      SELECT DISTINCT tube_ext.st_startpoint AS p FROM tube_ext
      UNION 
      SELECT DISTINCT tube_ext.st_endpoint AS p FROM tube_ext
   ) foo
   GROUP BY foo.p;

-- Link the nodes to the lines 

DROP TABLE IF EXISTS tube_network;
  
CREATE TABLE tube_network AS
   SELECT a.*, b.id as start_id, c.id as end_id
   FROM tube_ext AS a
      JOIN tube_node AS b ON a.st_startpoint = b.the_geom
      JOIN tube_node AS c ON a.st_endpoint = c.the_geom;

-- Add a serial and make it the primary key

ALTER TABLE tube_network ADD COLUMN id SERIAL;
ALTER TABLE tube_network ADD CONSTRAINT tube_network_pk PRIMARY KEY (id);
