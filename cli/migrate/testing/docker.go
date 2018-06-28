package testing

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"math/rand"
	"strconv"
	"strings"
	"testing"
	"time"

	dockertypes "github.com/docker/docker/api/types"
	dockercontainer "github.com/docker/docker/api/types/container"
	dockernetwork "github.com/docker/docker/api/types/network"
	dockerclient "github.com/docker/docker/client"
	"github.com/docker/go-connections/nat"
)

// DockerContainer implements Instance interface
type DockerContainer struct {
	t                  testing.TB
	client             *dockerclient.Client
	ImageName          string
	ENV                []string
	Cmd                []string
	exposePort         int
	ContainerID        string
	ContainerName      string
	ContainerJSON      dockertypes.ContainerJSON
	containerInspected bool
	keepForDebugging   bool
}

func NewDockerContainer(t testing.TB, image string, env []string, cmd []string, port int) (*DockerContainer, error) {
	c, err := dockerclient.NewEnvClient()
	if err != nil {
		return nil, err
	}

	if cmd == nil {
		cmd = make([]string, 0)
	}

	contr := &DockerContainer{
		t:          t,
		client:     c,
		ImageName:  image,
		ENV:        env,
		Cmd:        cmd,
		exposePort: port,
	}

	if err := contr.PullImage(); err != nil {
		return nil, err
	}

	if err := contr.Start(); err != nil {
		return nil, err
	}

	return contr, nil
}

func (d *DockerContainer) PullImage() error {
	if d == nil {
		return errors.New("Cannot pull image on a nil *DockerContainer")
	}
	d.t.Logf("Docker: Pull image %v", d.ImageName)
	r, err := d.client.ImagePull(context.Background(), d.ImageName, dockertypes.ImagePullOptions{})
	if err != nil {
		return err
	}
	defer r.Close()

	// read output and log relevant lines
	bf := bufio.NewScanner(r)
	for bf.Scan() {
		var resp dockerImagePullOutput
		if err := json.Unmarshal(bf.Bytes(), &resp); err != nil {
			return err
		}
		if strings.HasPrefix(resp.Status, "Status: ") {
			d.t.Logf("Docker: %v", resp.Status)
		}
	}
	return bf.Err()
}

func (d *DockerContainer) Start() error {
	if d == nil {
		return errors.New("Cannot start a nil *DockerContainer")
	}

	containerName := fmt.Sprintf("hasura_migrate_test_%s", pseudoRandStr(10))

	// create container first
	dockerconfig := &dockercontainer.Config{
		Image:  d.ImageName,
		Labels: map[string]string{"migrate_test": "true"},
		Env:    d.ENV,
		Cmd:    d.Cmd,
	}
	if d.exposePort != 0 {
		dockerconfig.ExposedPorts = nat.PortSet{
			"8080/tcp": struct{}{},
		}
	}
	resp, err := d.client.ContainerCreate(context.Background(),
		dockerconfig,
		&dockercontainer.HostConfig{
			PublishAllPorts: true,
		},
		&dockernetwork.NetworkingConfig{},
		containerName)
	if err != nil {
		return err
	}

	d.ContainerID = resp.ID
	d.ContainerName = containerName

	// then start it
	if err := d.client.ContainerStart(context.Background(), resp.ID, dockertypes.ContainerStartOptions{}); err != nil {
		return err
	}

	d.t.Logf("Docker: Started container %v (%v) for image %v listening at %v:%v", resp.ID[0:12], containerName, d.ImageName, d.Host(), d.Port())
	for _, v := range resp.Warnings {
		d.t.Logf("Docker: Warning: %v", v)
	}
	return nil
}

func (d *DockerContainer) KeepForDebugging() {
	if d == nil {
		return
	}

	d.keepForDebugging = true
}

func (d *DockerContainer) Remove() error {
	if d == nil {
		return errors.New("Cannot remove a nil *DockerContainer")
	}

	if d.keepForDebugging {
		return nil
	}

	if len(d.ContainerID) == 0 {
		return errors.New("missing ContainerID")
	}
	if err := d.client.ContainerRemove(context.Background(), d.ContainerID,
		dockertypes.ContainerRemoveOptions{
			Force: true,
		}); err != nil {
		d.t.Log(err)
		return err
	}
	d.t.Logf("Docker: Removed %v", d.ContainerName)
	return nil
}

func (d *DockerContainer) Inspect() error {
	if d == nil {
		return errors.New("Cannot inspect a nil *DockerContainer")
	}

	if len(d.ContainerID) == 0 {
		return errors.New("missing ContainerID")
	}
	resp, err := d.client.ContainerInspect(context.Background(), d.ContainerID)
	if err != nil {
		return err
	}

	d.ContainerJSON = resp
	d.containerInspected = true
	return nil
}

func (d *DockerContainer) Logs() (io.ReadCloser, error) {
	if d == nil {
		return nil, errors.New("Cannot view logs for a nil *DockerContainer")
	}
	if len(d.ContainerID) == 0 {
		return nil, errors.New("missing ContainerID")
	}

	return d.client.ContainerLogs(context.Background(), d.ContainerID, dockertypes.ContainerLogsOptions{
		ShowStdout: true,
		ShowStderr: true,
	})
}

func (d *DockerContainer) portMapping(selectFirst bool, cPort int) (containerPort uint, hostIP string, hostPort uint, err error) {
	if !d.containerInspected {
		if err := d.Inspect(); err != nil {
			d.t.Fatal(err)
		}
	}
	for port, bindings := range d.ContainerJSON.NetworkSettings.Ports {
		if !selectFirst && port.Int() != cPort {
			// Skip ahead until we find the port we want
			continue
		}
		for _, binding := range bindings {

			hostPortUint, err := strconv.ParseUint(binding.HostPort, 10, 64)
			if err != nil {
				return 0, "", 0, err
			}

			return uint(port.Int()), binding.HostIP, uint(hostPortUint), nil
		}
	}

	if selectFirst {
		return 0, "", 0, errors.New("no port binding")
	}
	return 0, "", 0, errors.New("specified port not bound")
}

func (d *DockerContainer) Host() string {
	if d == nil {
		panic("Cannot get host for a nil *DockerContainer")
	}
	_, hostIP, _, err := d.portMapping(true, -1)
	if err != nil {
		d.t.Fatal(err)
	}

	if hostIP == "0.0.0.0" {
		return "127.0.0.1"
	} else {
		return hostIP
	}
}

func (d *DockerContainer) Port() uint {
	if d == nil {
		panic("Cannot get port for a nil *DockerContainer")
	}
	_, _, port, err := d.portMapping(true, -1)
	if err != nil {
		d.t.Fatal(err)
	}
	return port
}

func (d *DockerContainer) PortFor(cPort int) uint {
	if d == nil {
		panic("Cannot get port for a nil *DockerContainer")
	}
	_, _, port, err := d.portMapping(false, cPort)
	if err != nil {
		d.t.Fatal(err)
	}
	return port
}

func (d *DockerContainer) NetworkSettings() dockertypes.NetworkSettings {
	if d == nil {
		panic("Cannot get network settings for a nil *DockerContainer")
	}
	netSettings := d.ContainerJSON.NetworkSettings
	return *netSettings
}

type dockerImagePullOutput struct {
	Status          string `json:"status"`
	ProgressDetails struct {
		Current int `json:"current"`
		Total   int `json:"total"`
	} `json:"progressDetail"`
	Id       string `json:"id"`
	Progress string `json:"progress"`
}

func init() {
	rand.Seed(time.Now().UnixNano())
}

func pseudoRandStr(n int) string {
	var letterRunes = []rune("abcdefghijklmnopqrstuvwxyz0123456789")
	b := make([]rune, n)
	for i := range b {
		b[i] = letterRunes[rand.Intn(len(letterRunes))]
	}
	return string(b)
}
