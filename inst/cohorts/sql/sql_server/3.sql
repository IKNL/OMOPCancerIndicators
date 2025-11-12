  INSERT INTO results.PBCR_breast_cancer_cohorts
    (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
  SELECT DISTINCT
    3 AS cohort_definition_id,
    co.person_id AS subject_id,
    co.condition_start_date AS cohort_start_date,
    co.condition_start_date AS cohort_end_date
  FROM omopcdm.condition_occurrence co
  JOIN omopcdm.person pe
    ON co.person_id = pe.person_id
  JOIN omopcdm.concept_ancestor ca
    ON ca.descendant_concept_id = co.condition_concept_id
    JOIN (
    WITH included_measurements AS (
      SELECT descendant_concept_id AS measurement_concept_id
      FROM omopcdm.concept_ancestor
      WHERE ancestor_concept_id IN (1633306)
    ),
    excluded_measurements AS (
      SELECT descendant_concept_id AS measurement_concept_id
      FROM omopcdm.concept_ancestor
      WHERE ancestor_concept_id IN (1635199, 1634252)
    ),
    allowed_measurements AS (
      SELECT measurement_concept_id
      FROM included_measurements
      WHERE measurement_concept_id NOT IN (SELECT measurement_concept_id FROM excluded_measurements)
    )
    SELECT m.person_id, m.measurement_date, m.measurement_concept_id
    FROM omopcdm.measurement m
    JOIN omopcdm.condition_occurrence co2
      ON co2.person_id = m.person_id
     AND m.measurement_event_id = co2.condition_occurrence_id
    JOIN allowed_measurements am
      ON am.measurement_concept_id = m.measurement_concept_id
    WHERE EXTRACT(YEAR FROM co2.condition_start_date) in (2019)
  ) m
  ON m.person_id = co.person_id
  WHERE ca.ancestor_concept_id IN (4112853)
    AND co.condition_concept_id NOT IN (44500370,4301516,36543333,36564967,36403034,36403041,36403047,36403017,4001315,4001670,36403027,36403064,36403075,36517431,36529758,36537608,36546703,36557659,36717217,42512152,42512201,44499447,44499563,44499751,44500653,44501092,44501151,44501152,44501276,44501353,44501356,44501446,44501633,44501854,44501955,44501957,44502178,44502714,44502954,44503030,44503556)
    AND pe.gender_concept_id in (8532)
    AND EXTRACT(YEAR FROM co.condition_start_date) in (2019)
      AND m.measurement_date BETWEEN co.condition_start_date - INTERVAL '30' DAY
  AND co.condition_start_date + INTERVAL '30' DAY;