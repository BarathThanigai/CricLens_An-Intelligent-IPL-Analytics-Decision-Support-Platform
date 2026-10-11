# Data input

Place one or more ball-by-ball CSV files in `data/raw/`. CricLens reads the first CSV it finds and stores a reusable standardized RDS cache in `data/processed/`.

Required: match ID, innings, batting team, batter (or striker), and runs scored. Useful for full features: season or date, over, legal-ball indicator, balls faced, bowler, extras, wicket kind, dismissed player, venue, match winner, toss winner/decision, result type, and target. Common aliases are recognized, including `match_id`/`id`, `runs_batter`/`batter_runs`/`batsman_runs`, `runs_total`/`total_runs`, `match_won_by`/`winner`, and `ground`/`venue`.

Over numbering is normalized to 1-based when the input contains over 0. Where a season label is in `YYYY/YY` form, it is mapped to the following calendar season; dates are used as fallback. Bowling figures depend on legal-ball, bowler-run, and wicket fields. Missing features are left unavailable rather than fabricated.

The bundled `IPL.csv` is a real ball-by-ball dataset supplied in the user-provided project archive. It remains in `data/raw/`; its actual coverage is computed by CricLens at runtime. Unit-test fixtures are synthetic and are explicitly labelled in the test file.
