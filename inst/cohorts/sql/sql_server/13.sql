  INSERT INTO results.PBCR
    (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
  SELECT DISTINCT
    13 AS cohort_definition_id,
    co.person_id AS subject_id,
    co.condition_start_date AS cohort_start_date,
    co.condition_start_date AS cohort_end_date
  FROM omopcdm.condition_occurrence co
  JOIN omopcdm.person pe
    ON co.person_id = pe.person_id
  JOIN omopcdm.concept_ancestor ca
    ON ca.descendant_concept_id = co.condition_concept_id
  
  WHERE ca.ancestor_concept_id IN (200051,4181351)
    AND co.condition_concept_id NOT IN (-1)
    AND pe.gender_concept_id IN (8532)
    AND EXTRACT(YEAR FROM co.condition_start_date) IN (2019)
    ;