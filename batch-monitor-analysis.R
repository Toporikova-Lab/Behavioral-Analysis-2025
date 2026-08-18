# ============================================================
# Batch Monitor Data Processor - RUN SCRIPT.
#
# Sources the function definitions, then runs the actual analysis.
# Run this deliberately (not by leaving it open and re-sourcing the
# whole project) since the block at the bottom kicks off a full batch
# run across every group folder.
# ============================================================

source('R/batch-monitor-function.R')

# base_dir points at the Box Drive location for the RNA-seq experiment data.
base_dir <- "/Users/isabelduarte/Library/CloudStorage/Box-Box/Summer2025-2026_Circadian_Students/Spider behavioral data and analysis/Locomotor activity monitor data/MW behavior for RNA-seq experiment/data"

# All output (plots, CSVs, flags) goes directly to Box Drive rather than
# a local ./output folder, so it's automatically synced/shared like the
# rest of this project's data.
output_root <- "/Users/isabelduarte/Library/CloudStorage/Box-Box/Summer2025-2026_Circadian_Students/Spider behavioral data and analysis/Locomotor activity monitor analysis"

# groups is auto-discovered since subfolder names don't follow the old
# LC_YYYY-MM-DD_N convention and new folders get added over time.
groups <- list.dirs(base_dir, recursive = FALSE, full.names = FALSE)
groups <- groups[!str_starts(groups, "\\.")]  # drop hidden/system folders

# Sample/dissection timing (ZT or CT at sampling, always the spider's
# last recorded day) - used to draw a dissection-time line on rasters.
sample_periods <- load_sample_periods("MW_LD_DD_periods.csv")

# Quick check of how many days each file covers (fast, no plots):
lengths <- check_monitor_lengths(base_dir, groups, output_root = output_root)
View(lengths)

# Preview the single detected DD-start day per file before trusting "auto"
# in the full run (used for dd_days trimming and the peak-period split).
dd_start_preview <- check_dd_start_days(base_dir, groups, output_root = output_root)
View(dd_start_preview)

# Preview every detected phase per file - check this for re-entrainment
# files with more than one LD/DD transition before trusting the full run.
segments_preview <- check_light_segments(base_dir, groups, output_root = output_root)
View(segments_preview)

#we need this test
test_flags <- run_batch_analysis(
  base_dir            = base_dir,
  group_folders       = groups[1],   # just the first group
  section2_start_day  = "auto",
  dd_days             = NULL,
  rasterplots_only    = FALSE,
  dedupe_checkpoints  = TRUE,
  output_root         = output_root,
  sample_periods      = sample_periods,
  zt_0                = "auto"
)

# Full batch run. section2_start_day = "auto" detects each file's DD
# start individually; files with 3+ real phases get numbered segment
# files, a combined raster+periodogram image, and a peaks.csv instead of
# the usual _LD.csv/_DD.csv/_combined.png. dd_days = NULL analyzes the
# full file - these recordings run 7-20 days, short enough that there's
# no need to cap the window (and capping risks cutting off a real phase
# transition). dedupe_checkpoints skips superseded re-export files
# automatically.
flags <- run_batch_analysis(
  base_dir            = base_dir,
  group_folders       = groups,
  section2_start_day  = "auto",
  dd_days             = NULL,
  rasterplots_only    = FALSE,
  dedupe_checkpoints  = TRUE,
  output_root         = output_root,
  sample_periods      = MW_LD_DD_periods,
  zt_0                = "auto",
  zt0_lookup          = zt0_lookup
)
View(flags)