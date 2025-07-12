BEGIN;

CREATE VIEW coastal.rolling365 AS
WITH cte AS (SELECT
	b.site_id,
	b.site_description,
	count(*) AS number_of_samples,
	coastal.hazen90(a.enterococci_cfu_per_100ml_numeric) AS hazen90,
	coastal.hazen95(a.enterococci_cfu_per_100ml_numeric) AS hazen95
FROM coastal.results_view a
	JOIN coastal.sites b ON a.site_id = b.site_id
GROUP BY b.site_id, b.site_description
ORDER BY b.site_description)
SELECT
	*,
	CASE
		WHEN number_of_samples < 10 THEN 'Not enough samples'
		WHEN hazen95 <= 100 THEN 'Excellent'
		WHEN hazen95 <= 200 THEN 'Good'
		WHEN hazen95 > 200 AND hazen90 > 185 THEN 'Poor'
		WHEN hazen95 > 200 AND hazen90 < 185 THEN 'Sufficient'
	END AS water_quality_category
FROM cte;

SELECT * FROM coastal.rolling365;
ROLLBACK;
COMMIT;
