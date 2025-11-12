  INSERT INTO results.PBCR_breast_cancer_cohorts
    (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
  SELECT DISTINCT
    12 AS cohort_definition_id,
    co.person_id AS subject_id,
    co.condition_start_date AS cohort_start_date,
    co.condition_start_date AS cohort_end_date
  FROM omopcdm.condition_occurrence co
  JOIN omopcdm.person pe
    ON co.person_id = pe.person_id
  JOIN omopcdm.concept_ancestor ca
    ON ca.descendant_concept_id = co.condition_concept_id
    INNER JOIN omopcdm.measurement m
    ON m.person_id = co.person_id
    AND m.measurement_concept_id IN (35918829)
    AND m.value_as_concept_id IN (35919593)
    AND m.measurement_date BETWEEN co.condition_start_date - INTERVAL '30' DAY
    AND co.condition_start_date + INTERVAL '30' DAY
  WHERE ca.ancestor_concept_id IN (4112853)
    AND co.condition_concept_id NOT IN (44500370,4301516,36543333,36564967,36403034,36403041,36403047,36403017,4001315,4001670,36403027,36403064,36403075,36517431,36529758,36537608,36546703,36557659,36717217,42512152,42512201,44499447,44499563,44499751,44500653,44501092,44501151,44501152,44501276,44501353,44501356,44501446,44501633,44501854,44501955,44501957,44502178,44502714,44502954,44503030,44503556)
    AND pe.gender_concept_id in (8532)
    AND EXTRACT(YEAR FROM co.condition_start_date) in (2019)
    ;