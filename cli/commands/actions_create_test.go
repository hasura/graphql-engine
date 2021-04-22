package commands

// TODO: we should make this test check the user actual user interaction
// 		this can be done with some changes in how we get input from the user
/*
var _ = Describe("actions_create", func() {

	var dirName string
	var session *Session
	var teardown func()
	BeforeEach(func() {
		dirName = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraVersion)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", dirName},
		})
		editEndpointInConfig(filepath.Join(dirName, defaultConfigFilename), hgeEndpoint)

		teardown = func() {
			session.Kill()
			os.RemoveAll(dirName)
			teardownHGE()
		}
	})

	AfterEach(func() {
		teardown()
	})
	Context("actions create tests", func() {
		data := `type Mutation {
			# Define your action as a mutation here
			action1 (arg1: SampleInput!): SampleOutput
		}

		type SampleOutput {
			accessToken: String!
		}

		input SampleInput {
			username: String!
			password: String!
		}`
		It("should create a new action action1 ", func() {
			filePath := filepath.Join(dirName, "action1.graphql")
			file, err := os.Create(filePath)
			if err != nil {
				fmt.Println(err)
			}
			_, err = file.Write([]byte(data))
			if err != nil {
				fmt.Println(err)
			}
			file.Close()
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"actions", "create", "action1", "--file", filePath},
				WorkingDirectory: dirName,
			})
			wantKeywordList := []string{
				".*action created*.",
			}

			for _, keyword := range wantKeywordList {
				Eventually(session, 60*40).Should(Say(keyword))
			}
			Eventually(session, 60*40).Should(Exit(0))
		})
	})
})
*/
