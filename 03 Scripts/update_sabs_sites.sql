BEGIN;

SELECT * FROM coastal.sites;

-- LIMS sites not in WALAB
INSERT INTO coastal.sites (site_id, site_description, category, active)
VALUES
	('CN02A', 'TABLE BAY DOCKS BREAKWATER END', 'Coastal Monitoring Point', false),
	('CN03', 'GRANGER BAY WEST BEACH', 'Coastal Monitoring Point', false),
	('CN05A', 'GREEN POINT OPPOSITE PARK ROAD', 'Coastal Monitoring Point', false),
	('CN06A', 'ROCKLANDS OPPOSITE SHOREHAM FLATS', 'Coastal Monitoring Point', false),
	('CN08I', 'SUNSET BEACH TIDAL POOL INSIDE', 'Recreational Node', false),
	('CN12B', 'CAMPS BAY TIDAL POOL NEAR PUMP STATION', 'Recreational Node', false),
	('CN15', 'BAKOVEN BUNGALOWS NW ROCKS', 'Recreational Node', false),
	('CN20O', 'MAIDENS COVE TIDAL POOL 2 OUTSIDE', 'Coastal Monitoring Point', false),
	('CN32', 'OUTSIDE THREE ANCHOR BAY', 'Coastal Monitoring Point', false),
	('CS01', 'KALK BAY ROCKS NEAR TIDAL POOL', 'Coastal Monitoring Point', false),
	('CS05', 'OLD SANDOWN HOTEL SITE OFF ROCKS', 'Coastal Monitoring Point', false),
	('CS08', 'LIFEBOX 21', 'Coastal Monitoring Point', false),
	('CS12', 'LIFEBOX 30', 'Coastal Monitoring Point', false),
	('CS24E', 'MITCHELLS PLAIN EAST STW SURF 50M EAST', 'Coastal Monitoring Point', false),
	('CS24W', 'MITCHELLS PLAIN EAST STW SURF 50M WEST', 'Coastal Monitoring Point', false),
	('XCN03A', 'LLANDUDNO BEACH AT CONFLUENCE WITH STREAM', 'Coastal Monitoring Point', false),
	('XCN15', 'MELKBOSSTRAND BEACH SOUTH', 'Recreational Node', false),
	('XCS33', 'LOURENS RIVER SEA NEAR MOUTH', 'Coastal Monitoring Point', false);

SAVEPOINT lims;

-- Can these be mapped to current sites?

-- SABS Weekly Samples
INSERT INTO coastal.sites (site_id, site_description, category, active)
VALUES
	('ICS01', 'DIEP RIVER ESTUARY MOUTH', 'Coastal Monitoring Point', false),
	('ICS03', 'CAMPS BAY TIDAL POOL STREAM', 'Coastal Monitoring Point', false),
	('ICS06', 'GLENCAIRN ELSA RIVER MOUTH', 'Coastal Monitoring Point', false),
	('ICS09', 'GORDONS BAY PARKING AREA', 'Coastal Monitoring Point', false),
	('ICS08', 'ZANDVLEI MOUTH', 'Coastal Monitoring Point', false),
	('ICS14', 'SAUNDERS ROCKS', 'Coastal Monitoring Point', false);

SAVEPOINT sabs;

-- Daily samples for SABS
INSERT INTO coastal.sites (site_id, site_description, category, active)
VALUES
	('ICS12', 'CAMPS BAY IN FRONT OF LIFESAVING TOWER', 'Coastal Monitoring Point', true);

SAVEPOINT sabs_daily;

-- Daily samples for Atlantic
INSERT INTO coastal.sites (site_id, site_description, category, active)
VALUES
	('ICS10', 'CAMPS BAY CENTRAL', 'Coastal Monitoring Point', true),
	('ICS15', 'MOUILLE POINT', 'Coastal Monitoring Point', false);

SAVEPOINT walab_daily;

SELECT * FROM coastal.sites
WHERE active
ORDER BY site_description;

ROLLBACK;
COMMIT;






