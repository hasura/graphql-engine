package fake

type FakeWriter struct {
}

func (fw *FakeWriter) Write(p []byte) (n int, err error) {
	return 0, nil
}
