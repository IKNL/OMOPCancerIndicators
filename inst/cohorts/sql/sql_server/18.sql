  INSERT INTO results.PBCR
    (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
  SELECT DISTINCT
    18 AS cohort_definition_id,
    co.person_id AS subject_id,
    co.condition_start_date AS cohort_start_date,
    co.condition_start_date AS cohort_end_date
  FROM omopcdm.condition_occurrence co
  JOIN omopcdm.person pe
    ON co.person_id = pe.person_id
  JOIN omopcdm.concept_ancestor ca
    ON ca.descendant_concept_id = co.condition_concept_id
    LEFT JOIN (
      SELECT 
          m.person_id,
          m.measurement_date,
          m.measurement_concept_id
      FROM omopcdm.measurement m
      JOIN omopcdm.condition_occurrence co2
        ON m.person_id = co2.person_id
       AND m.measurement_event_id = co2.condition_occurrence_id
      JOIN (
          SELECT descendant_concept_id AS measurement_concept_id
          FROM omopcdm.concept_ancestor
          WHERE ancestor_concept_id IN (1634306)
            AND descendant_concept_id NOT IN (
              SELECT descendant_concept_id
              FROM omopcdm.concept_ancestor
              WHERE ancestor_concept_id IN (-1)
            )
      ) allowed
        ON allowed.measurement_concept_id = m.measurement_concept_id
      WHERE EXTRACT(YEAR FROM co2.condition_start_date) IN (2019)
  ) m
    ON m.person_id = co.person_id
  WHERE ca.ancestor_concept_id IN (200051,4181351)
    AND co.condition_concept_id NOT IN (-1)
    AND pe.gender_concept_id IN (8532)
    AND EXTRACT(YEAR FROM co.condition_start_date) IN (2019)
    AND m.measurement_date BETWEEN co.condition_start_date - INTERVAL '30' DAY AND co.condition_start_date + INTERVAL '30' DAY;