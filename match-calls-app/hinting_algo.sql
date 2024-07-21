WITH distances AS (
    SELECT 
        r1.recording_ID AS recording_ID_1, 
        r2.recording_ID AS recording_ID_2, 
        r1.lat AS r1lat, 
        r1.lng AS r1lng, 
        r2.lat AS r2lat, 
        r2.lng AS r2lng,
        r1.measured_call_datetime AS mdt1, 
        r2.measured_call_datetime AS mdt2,
        r1.ground_truth_animal_ID AS animal1,
        r2.ground_truth_animal_ID AS animal2,
        CEIL(2 * 6371000 * ASIN(SQRT(
            SIN(RADIANS((r2.lat - r1.lat) / 2)) * SIN(RADIANS((r2.lat - r1.lat) / 2)) +
            COS(RADIANS(r1.lat)) * COS(RADIANS(r2.lat)) * 
            SIN(RADIANS((r2.lng - r1.lng) / 2)) * SIN(RADIANS((r2.lng - r1.lng) / 2))
        ))) AS distance_m, -- Haversine formula
        ABS(epoch(r1.measured_call_datetime) - epoch(r2.measured_call_datetime)) as datetime_diff
    FROM 
        (SELECT * FROM recs JOIN mics USING (mic_id)) r1
    JOIN 
        (SELECT * FROM recs JOIN mics USING (mic_id)) r2
    ON r1.recording_ID < r2.recording_ID  -- Make sure that for row j and row k to only include the pair <j,k> and not <k,j>. Also exclude rows where j=k.
),
ranked_distances AS (
    SELECT 
        distance_m, 
        recording_ID_1, 
        recording_ID_2,
        animal1,
        animal2,
        datetime_diff,
        DENSE_RANK() OVER (ORDER BY distance_m ASC) AS distance_rank, -- Dense rank such that rank number increments by 1 only whenever the distance_m changes.
        DENSE_RANK() OVER (ORDER BY datetime_diff ASC) AS time_rank
    FROM distances
    WHERE distance_m > 0 -- Exclude distance that is 0 since that implies it is the same recorder
)
SELECT
    recording_ID_1, 
    recording_ID_2,
    animal1,
    animal2,
    distance_m,
    datetime_diff
FROM 
    ranked_distances
WHERE 
    distance_rank IN (1,2) -- Get all rows where distance is minimum or second smallest
    AND time_rank IN (1,2); -- Get all rows where time difference is minimum or second smallest

