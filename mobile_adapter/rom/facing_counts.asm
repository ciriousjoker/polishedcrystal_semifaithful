; Native Facings OAM byte counts. audit_renderer.py checks every compiled entry.
MobileFacingOAMCounts:
	db 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16
	db 20, 20, 20, 20, 16, 8, 56, 64, 16, 16, 16, 16, 16, 16, 8, 8
	db 8, 8, 4, 4, 4, 64, 64, 16, 16, 8, 8, 8, 8, 32, 32, 28
	db 28, 28, 28, 8, 8, 8, 8, 24, 24, 36, 64, 64, 48, 48
ASSERT @ - MobileFacingOAMCounts == NUM_FACINGS
