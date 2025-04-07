BEGIN;

SELECT * FROM coastal.sites;

-- LIMS sites not in WALAB
INSERT INTO coastal.sites (site_id, site_description, category, geom)
VALUES
	('CN02A', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN03', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN05A', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN06A', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN08I', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN12B', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN15', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN20O', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN32', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CS01', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CS05', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CS08', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CS12', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CS24E', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CS24W', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('XCN03A', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('XCN15', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('XCS33', '', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326));

-- Can these be mapped to current sites?

-- SABS Weekly Samples
INSERT INTO coastal.sites (site_id, site_description, category, geom)
VALUES
	('ICS01', 'DIEP RIVER ESTUARY MOUTH', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('ICS03', 'CAMPS BAY TIDAL POOL STREAM', 'Recreational Node', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('ICS06', 'GLENCAIRN ELSA RIVER MOUTH', 'Recreational Node', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('ICS09', 'GORDONS BAY', 'Recreational Node', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('ICS08', 'ZANDVLEI MOUTH', 'Recreational Node', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CS12', 'STRANDFONTEIN POINT EAST OF TIDAL POOL', 'Recreational Node', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('ICS14', 'SAUNDERS ROCKS', 'Recreational Node', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326));

-- Daily samples for SABS
INSERT INTO coastal.sites (site_id, site_description, category, geom)
VALUES
	('ICS12', 'CAMPS BAY IN FRONT OF LIFESAVING TOWER', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326));

-- Daily samples for Atlantic
INSERT INTO coastal.sites (site_id, site_description, category, geom)
VALUES
	('ICS10', 'CAMPS BAY CENTRAL', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('ICS15', 'MOUILLE POINT', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326));

-- Daily samples for Strand (do we need to add sites or use current sites)
INSERT INTO coastal.sites (site_id, site_description, category, geom)
VALUES
	('CS', 'CAMPS BAY IN FRONT OF LIFESAVING TOWER', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326));

SELECT * FROM coastal.sites;

ROLLBACK;
COMMIT;






