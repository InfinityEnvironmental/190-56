BEGIN;

SELECT * FROM coastal.sites;

-- LIMS sites not in WALAB
INSERT INTO coastal.sites (site_id, site_description, category, geom)
VALUES
	('CN02A', 'TABLE BAY DOCKS BREAKWATER END', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN03', 'GRANGER BAY WEST BEACH', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN05A', 'GREEN POINT OPPOSITE PARK ROAD', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN06A', 'ROCKLANDS OPPOSITE SHOREHAM FLATS', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN08I', 'SUNSET BEACH TIDAL POOL INSIDE', 'Recreational Node', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN12B', 'CAMPS BAY TIDAL POOL NEAR PUMP STATION', 'Recreational Node', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN15', 'BAKOVEN BUNGALOWS NW ROCKS', 'Recreational Node', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN20O', 'MAIDENS COVE TIDAL POOL 2 OUTSIDE', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CN32', 'OUTSIDE THREE ANCHOR BAY', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CS01', 'KALK BAY ROCKS NEAR TIDAL POOL', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CS05', 'OLD SANDOWN HOTEL SITE OFF ROCKS', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CS08', 'LIFEBOX 21', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CS12', 'LIFEBOX 30', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CS24E', 'MITCHELLS PLAIN EAST STW SURF 50M EAST', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('CS24W', 'MITCHELLS PLAIN EAST STW SURF 50M WEST', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('XCN03A', 'LLANDUDNO BEACH AT CONFLUENCE WITH STREAM', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('XCN15', 'MELKBOSSTRAND BEACH SOUTH', 'Recreational Node', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('XCS33', 'LOURENS RIVER SEA NEAR MOUTH', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326));

-- Can these be mapped to current sites?

-- SABS Weekly Samples
INSERT INTO coastal.sites (site_id, site_description, category, geom)
VALUES
	('ICS01', 'DIEP RIVER ESTUARY MOUTH', 'Coastal Monitoring Point', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('ICS03', 'CAMPS BAY TIDAL POOL STREAM', 'Recreational Node', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('ICS06', 'GLENCAIRN ELSA RIVER MOUTH', 'Recreational Node', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
	('ICS09', 'GORDONS BAY PARKING AREA', 'Recreational Node', st_setsrid(st_makepoint(18.43298333, -34.1403), 4326)),
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






