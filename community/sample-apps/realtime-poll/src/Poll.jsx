import { useMutation, useQuery } from "@apollo/client";
import React, { useEffect, useState } from "react";
import { Button, Form } from "react-bootstrap";
import { Error, Loading } from "./Components";
import { MUTATION_VOTE, QUERY_GET_POLL } from "./GraphQL";
import { Result } from "./Result";

const PollQuestion = ({ poll, userId }) => {
  const defaultState = {
    optionId: "",
    pollId: poll.id,
    voteBtnText: "🗳 Vote",
    voteBtnStyle: "primary",
  };
  const [state, setState] = useState(defaultState);
  const [vote, { data, loading, error }] = useMutation(MUTATION_VOTE);

  const handlesubmitVote = (e) => {
    e.preventDefault();
    if (!state.optionId) {
      setState({
        voteBtnText: "✋ Select an option and try again",
        voteBtnStyle: "warning",
      });
      return;
    }

    setState({
      voteBtnText: "🗳️ Submitting",
      voteBtnStyle: "info",
    });

    vote({
      variables: {
        optionId: state.optionId,
        userId,
      },
    });
  };

  // To-do: use cleanup
  useEffect(() => {
    if (data) {
      setState({
        voteBtnText: "👍 Done",
        voteBtnStyle: "success",
      });

      //  Re-authorize to vote after 5 seconds
      let timer = setTimeout(() => {
        setState({
          voteBtnText: "🗳️ Vote",
          voteBtnStyle: "primary",
        });
      }, 5000);

      return () => clearTimeout(timer);
    }

    if (error) {
      setState({
        voteBtnText: "Error 😞 Try again",
        voteBtnStyle: "danger",
      });
    }
  }, [data, error]);

  const handleOptionChange = (e) => {
    const optionId = e.currentTarget.value;
    setState((prev) => ({ ...prev, optionId }));
  };

  return (
    <div className="textLeft">
      <h3>{poll.question}</h3>
      <Form
        className="pollForm textLeft"
        onSubmit={(e) => {
          handlesubmitVote(e);
        }}
      >
        {poll.options.map(({ id, text }) => (
          <Form.Check
            custom
            type="radio"
            name="voteCandidate"
            id={id}
            key={id}
            value={id}
            label={text}
            onChange={handleOptionChange}
          />
        ))}
        <Button
          className="voteBtn info"
          variant={state.voteBtnStyle}
          type="submit"
        >
          {state.voteBtnText}
        </Button>
      </Form>
    </div>
  );
};

export const Poll = ({ userId }) => {
  const { data, loading, error } = useQuery(QUERY_GET_POLL);

  if (loading) return <Loading />;
  if (error) return <Error message={error.message} />;

  return (
    <div className="container">
      {data?.poll.map((poll) => (
        <div key={poll.id} className="pollWrapper wd100">
          <div className="displayFlex">
            <div className="col-md-4 pollSlider">
              <PollQuestion poll={poll} userId={userId} />
            </div>
            <div className="col-md-8 pollresult">
              <Result pollId={poll.id} />
            </div>
          </div>
        </div>
      ))}
    </div>
  );
};
