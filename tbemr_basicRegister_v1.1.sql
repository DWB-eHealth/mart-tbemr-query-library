WITH baseline_disease_site_agg AS ( 
	SELECT 
	    blds.patient_id,
		blds.program_id,
		blds.id_baseline_template,
	    CASE
			WHEN string_agg(blds.baseline_disease_site,',' ORDER BY blds.baseline_disease_site) = 'Pulmonary' THEN 'Pulmonary'
			WHEN string_agg(blds.baseline_disease_site,',' ORDER BY blds.baseline_disease_site) = 'Extrapulmonary' THEN 'Extrapulmonary'		
			WHEN string_agg(blds.baseline_disease_site,',' ORDER BY blds.baseline_disease_site) = 'Extrapulmonary,Pulmonary' THEN 'Both'
			ELSE null
		END AS disease_site
	FROM baseline_disease_site AS blds
	GROUP BY blds.patient_id, blds.program_id, blds.id_baseline_template),
last_facility AS (
	WITH last_facility_sub AS (
		WITH treatment_initiation_facility AS (
			SELECT
				DISTINCT on (ti.patient_id, ti.program_id) ti.patient_id, 
				ti.program_id, 
				ti.tuberculosis_drug_treatment_start_date::date AS facility_date,
				ti.ti_treatment_facility_at_start AS facility
			FROM treatment_initiation_template AS ti
			WHERE ti.ti_treatment_facility_at_start is not null
			ORDER BY ti.patient_id, ti.program_id, ti.tuberculosis_drug_treatment_start_date DESC),
		hospital_admission_facililty AS (
			SELECT
				DISTINCT on (hant.patient_id, hant.program_id) hant.patient_id, 
				hant.program_id, 
				hant.han_hospital_admission_date::date AS facility_date,
				CASE
					WHEN hant.treatment_facility_name != 'Other' THEN hant.treatment_facility_name
					ELSE hant.other_treatment_facility_name
				END AS facility
			FROM hospital_admission_notification_template AS hant
			WHERE hant.treatment_facility_name is not null
			ORDER BY hant.patient_id, hant.program_id, hant.han_hospital_admission_date DESC),
		followup_facility AS (
			SELECT
				DISTINCT on (ft.patient_id, ft.program_id) ft.patient_id, 
				ft.program_id, 
				ft.followup_visit_date::date AS facility_date,
				CASE
					WHEN ft.treatment_facility_name != 'Other' THEN ft.treatment_facility_name
					ELSE ft.other_treatment_facility_name
				END AS facility
			FROM followup_template AS ft
			WHERE ft.treatment_facility_name is not null
			ORDER BY ft.patient_id, ft.program_id, ft.followup_visit_date DESC)
		SELECT * FROM treatment_initiation_facility
		UNION
		SELECT * FROM hospital_admission_facililty
		UNION 
		SELECT * FROM followup_facility
		ORDER BY patient_id, program_id, facility_date DESC),
	enrollment_facility AS (
		SELECT
			ppdd.patient_id,
			ppdd.program_id,
			ppdd.date_enrolled::date AS facility_date,
			pa."Registration_Facility" AS facility
		FROM program_attributes AS pa
		LEFT OUTER JOIN patient_program_data_default AS ppdd
			ON pa.patient_program_id = ppdd.patient_program_id
		ORDER BY ppdd.patient_id, ppdd.program_id, ppdd.date_enrolled DESC)		
	SELECT 
		ef.patient_id,
		ef.program_id,
		CASE
			WHEN lf.facility IS NOT NULL THEN lf.facility
			ELSE ef.facility
		END AS last_facility
	FROM enrollment_facility AS ef
	LEFT OUTER JOIN (
		SELECT 
			DISTINCT ON (patient_id, program_id) patient_id, 
			program_id, 
			facility 
		FROM last_facility_sub 
		WHERE facility IS NOT NULL 
		ORDER BY patient_id, program_id, facility_date DESC) AS lf
	ON ef.patient_id = lf.patient_id AND ef.program_id = lf.program_id),
dlm_start_date AS (
    SELECT 
        DISTINCT ON (mdd.patient_program_id) mdd.start_date AS dlm_start_date,
		mdd.patient_program_id AS patient_program_id
    FROM medication_data_default AS mdd
	WHERE mdd.coded_drug_name = 'Delamanid (Dlm)'
	ORDER BY mdd.patient_program_id, mdd.start_date),
bdq_start_date AS (
    SELECT 
        DISTINCT ON (mdd.patient_program_id) mdd.start_date AS bdq_start_date,
		mdd.patient_program_id AS patient_program_id
    FROM medication_data_default AS mdd
    WHERE mdd.coded_drug_name = 'Bedaquiline (Bdq)'
    ORDER BY mdd.patient_program_id, mdd.start_date),
initial_drug_regimen AS (
	SELECT
        idr.patient_program_id,
        MAX (CASE WHEN idr.drug = 'Isoniazid (H)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS H,
        MAX (CASE WHEN idr.drug = 'Rifampicin (R)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS R,
        MAX (CASE WHEN idr.drug = 'Ethambutol (E)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS E,
        MAX (CASE WHEN idr.drug = 'Pyrazinamide (Z)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Z,
        MAX (CASE WHEN idr.drug = 'Streptomycin (S)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS S,
        MAX (CASE WHEN idr.drug = 'Amikacin (Am)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Am,
        MAX (CASE WHEN idr.drug = 'Kanamycin (Km)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Km,
        MAX (CASE WHEN idr.drug = 'Capreomycin (Cm)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Cm,
        MAX (CASE WHEN idr.drug = 'Levofloxacin (Lfx)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Lfx,
        MAX (CASE WHEN idr.drug = 'Moxifloxacin (Mfx)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Mfx,
        MAX (CASE WHEN idr.drug = 'Prothionamide (Pto)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Pto,
        MAX (CASE WHEN idr.drug = 'Ethionamide (Eto)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Eto,
        MAX (CASE WHEN idr.drug = 'Cycloserine (Cs)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Cs,
        MAX (CASE WHEN idr.drug = 'Terizidone (Trd)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Trd,
        MAX (CASE WHEN idr.drug = 'Para-aminosalicylic acid (PAS)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS PAS,
        MAX (CASE WHEN idr.drug = 'Para-aminosalicylic acid - sodium (PAS-Na)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS PAS_Na,
        MAX (CASE WHEN idr.drug = 'Bedaquiline (Bdq)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Bdq,
        MAX (CASE WHEN idr.drug = 'Delamanid (Dlm)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Dlm,
        MAX (CASE WHEN idr.drug = 'Linezolid (Lzd)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Lzd,
        MAX (CASE WHEN idr.drug = 'Clofazimine (Cfz)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Cfz,
        MAX (CASE WHEN idr.drug = 'Imipenem/Cilastatin (Imp/Cln)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Imp_Cln,
        MAX (CASE WHEN idr.drug = 'Amoxicillin/Clavulanate (Amx/Clv)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Amx_Clv,
        MAX (CASE WHEN idr.drug = 'Meropenem (Mpm)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Mpm,
        MAX (CASE WHEN idr.drug = 'Pretomanid (Pa)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Pa,
        MAX (CASE WHEN idr.drug = 'Rifapentine (Rpt or P)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS Rpt_or_P
    FROM (SELECT 
			mdd.patient_program_id AS patient_program_id,
	        mdd.coded_drug_name AS drug,
	        mdd.dose AS dose,
	        mdd.dose_units AS dose_unit
		FROM medication_data_default AS mdd
		LEFT OUTER JOIN treatment_initiation_template AS ti
			ON mdd.patient_id = ti.patient_id and mdd.patient_program_name = ti.program_name
		WHERE mdd.start_date BETWEEN (ti.tuberculosis_drug_treatment_start_date - 7) and (ti.tuberculosis_drug_treatment_start_date + 7) and mdd.coded_drug_name IS NOT NULL
		GROUP BY mdd.coded_drug_name, mdd.patient_program_id, mdd.dose, mdd.dose_units) AS idr
	GROUP BY idr.patient_program_id),
latest_hiv_result AS (
    SELECT 
        distinct on (lrs.patient_id, lrs.program_id) lrs.lab_hiv_test_result AS hiv_result,
        lrs.patient_id,
        lrs.program_id
    FROM lab_results_serology_template AS lrs
    WHERE lrs.lab_hiv_test_result is not NULL
    ORDER BY lrs.patient_id, lrs.program_id, lrs.specimen_collection_date), 
latest_hep_b_result AS (
    SELECT 
        distinct on (lrs.patient_id, lrs.program_id) lrs.lab_hepatitis_b_antigen_test_result AS hep_b_result,
        lrs.patient_id,
        lrs.program_id
    FROM lab_results_serology_template AS lrs
    WHERE lrs.lab_hepatitis_b_antigen_test_result is not NULL
    ORDER BY lrs.patient_id, lrs.program_id, lrs.specimen_collection_date),
latest_hep_c_result AS (
    SELECT 
        distinct on (lrs.patient_id, lrs.program_id) lrs.lab_hepatitis_c_antibody_test_result AS Hep_C_result,
        lrs.patient_id,
        lrs.program_id
    FROM lab_results_serology_template AS lrs
    WHERE lrs.lab_hepatitis_c_antibody_test_result is not NULL
    ORDER BY lrs.patient_id, lrs.program_id, lrs.specimen_collection_date),
latest_followup AS (
    SELECT 
        DISTINCT ON (patient_id, program_id) patient_id, 
        program_id,
        return_visit_date
    FROM followup_template
    GROUP by patient_id, program_id, return_visit_date, followup_visit_date
    ORDER by patient_id, program_id, followup_visit_date),
baseline_pos AS (
		WITH pos_culture_baseline_fuzzy AS (
			WITH culture_positive AS (
				SELECT 
					bcrd.patient_id,
					bcrd.program_id,
					bcs.specimen_collection_date,
					bcrd.bacteriology_culture_results
				FROM bacteriology_culture_results_details AS bcrd
				LEFT OUTER JOIN bacteriology_concept_set AS bcs
					ON bcrd.id_bacteriology_concept_set = bcs.id_bacteriology_concept_set
				WHERE bcrd.bacteriology_culture_results = 'Positive for M. tuberculosis')
			SELECT
				ti.patient_id,
				ti.program_id,
				ti.tuberculosis_drug_treatment_start_date,
				cp.specimen_collection_date,
				cp.bacteriology_culture_results,
				(cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) AS day_diff_pos
			FROM treatment_initiation_template AS ti
			LEFT OUTER JOIN culture_positive AS cp
				ON ti.patient_id = cp.patient_id AND ti.program_id = cp.program_id
			WHERE (cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) >= -90 
			AND (cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) <= 15
			ORDER BY cp.patient_id, (cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date)),
		neg_culture_baseline_fuzzy AS (
			WITH culture_negative AS (
				SELECT 
					bcrd.patient_id,
					bcrd.program_id,
					bcs.specimen_collection_date,
					bcrd.bacteriology_culture_results
				FROM bacteriology_culture_results_details AS bcrd
				LEFT OUTER JOIN bacteriology_concept_set AS bcs
					ON bcrd.id_bacteriology_concept_set = bcs.id_bacteriology_concept_set
				WHERE bcrd.bacteriology_culture_results = 'Negative for M. tuberculosis')
			SELECT
				ti.patient_id,
				ti.program_id,
				ti.tuberculosis_drug_treatment_start_date,
				cn.specimen_collection_date,
				cn.bacteriology_culture_results,
				(cn.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) AS day_diff_neg
			FROM treatment_initiation_template AS ti
			LEFT OUTER JOIN culture_negative AS cn
				ON ti.patient_id = cn.patient_id AND ti.program_id = cn.program_id
			WHERE (cn.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) >= -90 
				AND (cn.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) <= 15
			ORDER BY cn.patient_id, (cn.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date))	
		SELECT
			pcb.patient_id,
			pcb.program_id,
			pcb.tuberculosis_drug_treatment_start_date,
			pcb.specimen_collection_date AS closest_positive,
			ncb.specimen_collection_date AS closest_negative,
			CASE
				WHEN (pcb.tuberculosis_drug_treatment_start_date - pcb.specimen_collection_date) > (pcb.tuberculosis_drug_treatment_start_date - ncb.specimen_collection_date) THEN 'Y'
				ELSE NULL 
			END AS closer_negative
		FROM (SELECT 
			ROW_NUMBER() OVER (PARTITION BY pcbf.patient_id, pcbf.program_id ORDER BY abs(pcbf.day_diff_pos - 0) ASC) AS pos_bl,
			pcbf.*
		FROM pos_culture_baseline_fuzzy AS pcbf) AS pcb
		LEFT OUTER JOIN (SELECT 
			ROW_NUMBER() OVER (PARTITION BY ncbf.patient_id, ncbf.program_id ORDER BY abs(ncbf.day_diff_neg - 0) ASC) AS neg_bl,
			ncbf.*
		FROM neg_culture_baseline_fuzzy AS ncbf) ncb
			ON pcb.patient_id = ncb.patient_id AND pcb.program_id = ncb.program_id
		WHERE pcb.pos_bl = 1),
initial_cc AS (
	WITH baseline_pos AS (
		WITH pos_culture_baseline_fuzzy AS (
			WITH culture_positive AS (
				SELECT 
					bcrd.patient_id,
					bcrd.program_id,
					bcs.specimen_collection_date,
					bcrd.bacteriology_culture_results
				FROM bacteriology_culture_results_details AS bcrd
				LEFT OUTER JOIN bacteriology_concept_set AS bcs
					ON bcrd.id_bacteriology_concept_set = bcs.id_bacteriology_concept_set
				WHERE bcrd.bacteriology_culture_results = 'Positive for M. tuberculosis')
			SELECT
				ti.patient_id,
				ti.program_id,
				ti.tuberculosis_drug_treatment_start_date,
				cp.specimen_collection_date,
				cp.bacteriology_culture_results,
				(cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) AS day_diff_pos
			FROM treatment_initiation_template AS ti
			LEFT OUTER JOIN culture_positive AS cp
				ON ti.patient_id = cp.patient_id AND ti.program_id = cp.program_id
			WHERE (cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) >= -90 
			AND (cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) <= 15
			ORDER BY cp.patient_id, (cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date))	
		SELECT
			pcb.patient_id,
			pcb.program_id,
			pcb.tuberculosis_drug_treatment_start_date,
			pcb.specimen_collection_date AS closest_positive
		FROM (SELECT 
			ROW_NUMBER() OVER (PARTITION BY pcbf.patient_id, pcbf.program_id ORDER BY abs(pcbf.day_diff_pos - 0) ASC) AS pos_bl,
			pcbf.*
		FROM pos_culture_baseline_fuzzy AS pcbf) AS pcb
		WHERE pcb.pos_bl = 1),
	negative_results AS (
		SELECT
			bcrd.patient_id, 
			bcrd.program_id,
			bcs.specimen_collection_date::date as collection_date
		FROM bacteriology_culture_results_details AS bcrd
		LEFT OUTER JOIN bacteriology_concept_set AS bcs
			ON bcrd.id_bacteriology_concept_set = bcs.id_bacteriology_concept_set
		LEFT OUTER JOIN treatment_initiation_template AS ti
			ON bcrd.patient_id = ti.patient_id AND bcrd.program_id = ti.program_id
		WHERE bcrd.bacteriology_culture_results = 'Negative for M. tuberculosis' AND bcs.specimen_collection_date >= ti.tuberculosis_drug_treatment_start_date),
	positive_results AS (
		SELECT
			bcrd.patient_id, 
			bcrd.program_id,
			bcs.specimen_collection_date::date as collection_date
		FROM bacteriology_culture_results_details AS bcrd
		LEFT OUTER JOIN bacteriology_concept_set AS bcs
			ON bcrd.id_bacteriology_concept_set = bcs.id_bacteriology_concept_set
		LEFT OUTER JOIN treatment_initiation_template AS ti
			ON bcrd.patient_id = ti.patient_id AND bcrd.program_id = ti.program_id
		WHERE bcrd.bacteriology_culture_results = 'Positive for M. tuberculosis' AND bcs.specimen_collection_date >= ti.tuberculosis_drug_treatment_start_date),
	cc_date AS (	
		SELECT
			nr.patient_id,
			nr.program_id,
			nr.collection_date AS collection_date1,
			nr2.collection_date AS collection_date2,
			(nr2.collection_date - nr.collection_date) AS days_between,
			SUM(CASE 
				WHEN nr.patient_id = bp.patient_id AND nr.program_id = bp.program_id THEN 1
				ELSE 0
			END) AS pos_baseline,
			SUM(CASE
				WHEN pr.collection_date BETWEEN nr.collection_date AND nr2.collection_date THEN 1
				WHEN pr.collection_date = nr.collection_date THEN 1
				WHEN pr.collection_date = nr2.collection_date THEN 1
				ELSE 0
			END) AS pos_between,
			SUM(CASE
				WHEN bp.closest_positive < nr.collection_date THEN 1
				ELSE 0
			END) AS cc_after_pos,
			SUM(CASE
				WHEN pr.collection_date > nr2.collection_date THEN 1
				ELSE 0
			END) AS pos_after_cc
		FROM negative_results AS nr
		INNER JOIN negative_results AS nr2
			ON nr.patient_id = nr2.patient_id AND nr.program_id = nr2.program_id AND nr.collection_date != nr2.collection_date
		LEFT OUTER JOIN positive_results AS pr
			ON nr.patient_id = pr.patient_id AND nr.program_id = pr.program_ID
		LEFT OUTER JOIN baseline_pos AS bp
			ON nr.patient_id = bp.patient_id AND nr.program_id = bp.program_id
		WHERE (nr2.collection_date - nr.collection_date) > 15
		GROUP BY nr.patient_id, nr.program_id, nr.collection_date, nr2.collection_date
		ORDER BY nr.patient_id, nr.collection_date)
	SELECT
		DISTINCT ON (cd.patient_id, cd.program_id) cd.patient_id,
		cd.program_id,
		cd.collection_date1 AS initial_cc_date,
		CASE
			WHEN cd.pos_after_cc > 0 THEN 'Y'
			ELSE NULL 
		END AS revert_after_cc
	FROM cc_date AS cd 
	WHERE cd.pos_between = 0 AND cd.pos_baseline != 0 AND cd.cc_after_pos != 0
	ORDER BY cd.patient_id, cd.program_id, cd.collection_date1)
SELECT
	pa."Registration_Number" AS Registration_Number,
    ppv.program_name,
    pi."Patient_Identifier" AS EMR_ID,
    ppv.gender AS Sex,
    ppv.age_at_program AS Age_at_Registration,
    ppv.age_group_at_program AS Age_group_at_Registration,
    bl.baseline_who_registration_group AS WHO_Registration_Group,
    bl.category_iv_tuberculosis_classification AS History_of_Previously_Treated,
    blds.disease_site AS Baseline_Disease_Site,
	bl.baseline_mdr_tb_diagnosis_method AS MTB_Confirmed, 
    bl.baseline_drug_resistance AS DR_Resistance_Profile, 
    bl.baseline_subclassification_for_confimed_drug_resistant_case AS Subclassification_for_Confirmed,
    ti.tuberculosis_drug_treatment_start_date::date AS Treatment_Start_Date,
	ROUND((CASE 
        WHEN eot.tuberculosis_treatment_end_date is not null then (DATE_PART('day',(eot.tuberculosis_treatment_end_date::timestamp)-(ti.tuberculosis_drug_treatment_start_date::timestamp)))/365*12
        ELSE (DATE_PART('day',(now()::timestamp)-(ti.tuberculosis_drug_treatment_start_date::timestamp)))/365*12
    END)::NUMERIC,1) AS Treatment_Duration_months,
    ti.ti_second_line_regimen_drug_type AS Secondline_Treatment_Type,
	dsd.dlm_start_date::date AS Start_Date_Dlm,
    bsd.bdq_start_date::date AS Start_Date_Bdq,
	LEAST(dsd.dlm_start_date,bsd.bdq_start_date)::date AS Start_Date_Dlm_or_Bdq,
	CASE
		WHEN dsd.dlm_start_date BETWEEN (bsd.bdq_start_date::date - 7) AND (bsd.bdq_start_date::date + 7) THEN LEAST(dsd.dlm_start_date,bsd.bdq_start_date)::date
		ELSE null									  
	END AS Start_Date_Dlm_and_Bdq,
	lf.last_facility AS Last_Facility,
    idr.H,
    idr.R,
    idr.E,
    idr.Z,
    idr.S,
    idr.Am,
    idr.Km,
    idr.Cm,
    idr.Lfx,
    idr.Mfx,
    idr.Pto,
    idr.Eto,
    idr.Cs,
    idr.Trd,
    idr.PAS,
    idr.PAS_Na,
    idr.Bdq,
    idr.Dlm,
    idr.Lzd,
    idr.Cfz,
    idr.Imp_Cln,
    idr.Amx_Clv,
    idr.Mpm,
    idr.Pa,
    idr.Rpt_or_P,
    bl.baseline_hiv_serostatus_result AS HIV_Baseline,
	lhr.hiv_result AS HIV_Lab,
    CASE
        WHEN lhr.hiv_result =  'Positive' THEN 'HIV Positive'
		WHEN lhr.hiv_result = 'Negative' THEN 'HIV Negative'
		ELSE (CASE
			WHEN bl.baseline_hiv_serostatus_result = 'Positive' THEN 'HIV Positive'
			WHEN bl.baseline_hiv_serostatus_result = 'Negative' THEN 'HIV Negative'
			ELSE null
			END)
    END AS HIV_Status,
    CASE
        WHEN bl.baseline_hepatitis_b = 'False' THEN 'Negative'
        WHEN bl.baseline_hepatitis_b = 'True' THEN 'Positive'
        WHEN bl.baseline_hepatitis_b = 'Unknown' THEN 'Unknown'
        ELSE null
    END AS Hep_B_Baseline,
	lhbr.hep_b_result AS Hep_B_Lab,
	CASE
        WHEN lhbr.hep_b_result = 'Non-reactive' THEN 'Hep B Negative'
        WHEN lhbr.hep_b_result = 'Reactive' THEN 'Hep B Positive'
        ElSE (CASE
            WHEN bl.baseline_hepatitis_b = 'True' THEN 'Hep B Positive' 
            WHEN bl.baseline_hepatitis_b = 'False' THEN 'Hep B Negative'
            ELSE null
            END)
    END AS Hep_B_Status,
    CASE
        WHEN bl.baseline_hepatitis_c = 'True' THEN 'Positive'
        WHEN bl.baseline_hepatitis_c = 'False'THEN 'Negative'
        ELSE null
    END AS Hep_C_Baseline,	
    lhcr.hep_c_result AS "Hep C Lab",
	CASE
        WHEN lhcr.hep_c_result = 'Non-reactive' THEN 'Hep C Negative'
        WHEN lhcr.hep_c_result = 'Reactive' THEN 'Hep C Positive'
        ElSE (CASE
            WHEN bl.baseline_hepatitis_c = 'True' THEN 'Hep C Positive' 
            WHEN bl.baseline_hepatitis_c = 'False' THEN 'Hep C Negative'
            ELSE null
            END)
    END AS Hep_C_Status,
    CASE
        WHEN bl.diabetes_mellitus = 'False' THEN 'Negative'
        WHEN bl.diabetes_mellitus = 'True' THEN 'Positive'
        ELSE null
    END AS Diabetes_Baseline,
    CASE
    	WHEN bpos.patient_id IS NOT NULL THEN 'Y'
    	ELSE null
    END AS Positive_Culture_at_Baseline,
    bpos.closer_negative AS Negative_Culture_Closer_to_Baseline_Than_Positive_Culture,
    CASE
    	WHEN icc.patient_id IS NOT NULL THEN 'Y'
    	ELSE null
    END AS Culture_Conversion_for_positive_at_baseline_only,
    icc.initial_cc_date AS Initial_Culture_Conversion_Date,
    ROUND(((DATE_PART('day',(icc.initial_cc_date::timestamp)-(ti.tuberculosis_drug_treatment_start_date::timestamp)))/365*12)::NUMERIC,1) AS Months_to_Initial_Culture_Conversion_Date,
    icc.revert_after_cc AS Re_conversion_after_Initial_Culture_Conversion,
    eot.eot_outcome AS Outcome,
    eot.tuberculosis_treatment_end_date AS End_of_Treatment_Date,
    CASE
        WHEN lfu.return_visit_date IS NOT NULL THEN lfu.return_visit_date
        ELSE bl.return_visit_date
    END AS Next_Visit
FROM patient_program_view AS ppv 
LEFT OUTER JOIN treatment_initiation_template AS ti
	ON ppv.patient_id = ti.patient_id and ppv.program_id = ti.program_id
LEFT OUTER JOIN patient_identifier AS pi
    ON ppv.patient_id = pi.patient_id
LEFT OUTER JOIN program_attributes AS pa
    ON ppv.patient_program_id = pa.patient_program_id
LEFT OUTER JOIN baseline_template AS bl
    ON ppv.patient_id = bl.patient_id and ppv.program_id = bl.program_id
LEFT OUTER JOIN baseline_disease_site_agg AS blds
    ON ppv.patient_id = blds.patient_id and ppv.program_id = blds.program_id												  
LEFT OUTER JOIN dlm_start_date AS dsd
    ON ppv.patient_program_id = dsd.patient_program_id
LEFT OUTER JOIN bdq_start_date AS bsd
    ON ppv.patient_program_id = bsd.patient_program_id										  
LEFT OUTER JOIN last_facility AS lf
	ON ppv.patient_id = lf.patient_id and ppv.program_id = lf.program_id
LEFT OUTER JOIN initial_drug_regimen AS idr
    ON ppv.patient_program_id = idr.patient_program_id
LEFT OUTER JOIN latest_hiv_result AS lhr
    ON ppv.patient_id = lhr.patient_id and ppv.program_id = lhr.program_id
LEFT OUTER JOIN latest_hep_b_result AS lhbr
    ON ppv.patient_id = lhbr.patient_id and ppv.program_id = lhbr.program_id
LEFT OUTER JOIN latest_hep_c_result AS lhcr
    ON ppv.patient_id = lhcr.patient_id and ppv.program_id = lhcr.program_id
LEFT OUTER JOIN baseline_pos AS bpos
	ON ppv.patient_id = bpos.patient_id and ppv.program_id = bpos.program_id
LEFT OUTER JOIN initial_cc AS icc
	ON ppv.patient_id = icc.patient_id and ppv.program_id = icc.program_id
LEFT OUTER JOIN outcome_end_of_treatment_template AS eot
    ON ppv.patient_id = eot.patient_id and ppv.program_id = eot.program_id
LEFT OUTER JOIN latest_followup AS lfu
    ON ppv.patient_id = lfu.patient_id and ppv.program_id = lfu.program_id
WHERE ti.tuberculosis_drug_treatment_start_date IS NOT NULL
ORDER BY pi.patient_id