-- pet-report schema, version 2.
--
-- Widens the review backlog from "the model was unsure" to "the model was unsure OR the
-- moment is flagged concerning". Before this, a concerning moment (a safety sound, or a
-- scene the model called concerning) was counted by the Today badge but could not be
-- reached: it never entered the backlog, no facet selected it, and wellbeing has no column.
--
-- One resolved column rather than two raw ones, for the reason schema finding F1 already
-- gives: storing the answer keeps the 0.62 confidence threshold out of SQL entirely. A
-- second `concerning` column would also be a second derived value to keep in step at all
-- four write sites, and would put the same OR in both the index DDL and the query fragment.
--
-- Renaming rather than adding carries the uncertainty half of the backfill for free.
-- RENAME COLUMN rewrites the index's own predicate, so observations_needslook needs no
-- attention here.

ALTER TABLE observations RENAME COLUMN uncertain TO needs_look;

-- Backfill the concerning half, so upgrading does not depend on an operator remembering to
-- run `reproject`. This restates PetReport.Domain.Perception.isSafetySound, which is the
-- price of a migration that stands alone: the file is frozen once shipped and describes a
-- point in time, while the Haskell rule stays the single live definition.
UPDATE observations SET needs_look = 1
WHERE json_extract(perception, '$.scene.wellbeing') = 'concerning'
   OR (json_extract(perception, '$.kind') = 'sound'
       AND json_extract(perception, '$.sound') IN
           ('fire_alarm', 'smoke_detector', 'smoke_alarm', 'co_alarm', 'siren',
            'car_alarm', 'glass', 'shatter', 'breaking'));
