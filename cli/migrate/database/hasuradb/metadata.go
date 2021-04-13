package hasuradb

func (h *HasuraDB) EnableCheckMetadataConsistency(enabled bool) {
	h.config.enableCheckMetadataConsistency = enabled
}
