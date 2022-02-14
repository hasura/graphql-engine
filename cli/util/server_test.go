package util

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	"github.com/sirupsen/logrus"
)

func TestGetServerState(t *testing.T) {
	portV13, teardownV133 := testutil.StartHasura(t, "hasura/graphql-engine:v1.3.3")
	hgeEndpointV133 := fmt.Sprintf("http://localhost:%s", portV13)
	defer teardownV133()
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	defer teardown()
	type args struct {
		client        *httpc.Client
		endpoint      string
		hasMetadataV3 bool
		log           *logrus.Logger
	}
	tests := []struct {
		name string
		args args
	}{
		{
			"can generate server state for v1.x",
			args{

				testutil.NewHttpcClient(t, portV13, nil),
				fmt.Sprintf("%v/v1/query", hgeEndpointV133),
				false,
				logrus.New(),
			},
		},
		{
			"can generate server state for latest",
			args{

				testutil.NewHttpcClient(t, port, nil),
				fmt.Sprintf("%v/v1/metadata", hgeEndpoint),
				true,
				logrus.New(),
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := GetServerState(tt.args.client, tt.args.endpoint, tt.args.hasMetadataV3, tt.args.log)
			assert.Truef(t, got.UUID != "00000000-0000-0000-0000-000000000000" && got.UUID != "", "expected server UUID to be set got: ", got.UUID)
		})
	}
}
