import React, { useState, useEffect } from 'react';
import styles from './styles.module.scss';

const CloudDeployURLGenerationForm = () => {
  // URL state
  const [repoName, setRepoName] = useState('');
  const [branchName, setBranchName] = useState('');
  const [pathToDir, setPathToDir] = useState('');
  const [url, setUrl] = useState(null);
  // UI state
  const [isSubmittable, setIsSubmittable] = useState(false);
  const [isSubmitted, setIsSubmitted] = useState(false);

  // when the inputs change, check if the form is submittable
  useEffect(() => {
    repoName !== '' ? setIsSubmittable(true) : setIsSubmittable(false);
  }, [repoName, branchName, pathToDir]);

  // Handle form input change
  const handleChange = e => {
    const { name, value } = e.target;
    switch (name) {
      case 'github-repo':
        setRepoName(value);
        break;
      case 'github-branch':
        setBranchName(value);
        break;
      case 'mms-path':
        setPathToDir(value);
        break;
      default:
        break;
    }
  };

  // Handle 'form' submission
  const handleSubmit = () => {
    // check for empty fields
    if (repoName === '') {
      alert(
        'The repository name is empty. Please fill it out before proceeding.'
      );
      return;
    }
    // check for whitespace or spaces in the fields
    if (
      repoName.includes(' ') ||
      branchName.includes(' ') ||
      pathToDir.includes(' ')
    ) {
      alert(
        'One or more fields contain whitespace. Please remove it and try again.'
      );
      return;
    }
    const url =
      'https://cloud.hasura.io/deploy' +
      `?github_repo=${repoName}` +
      `&hasura_dir=${pathToDir !== '' ? pathToDir : '/'}` +
      (branchName !== '' ? `&branch=${branchName}` : '');

    setUrl(url);
    navigator.clipboard.writeText(url);
    setIsSubmitted(true);
  };

  const handleClear = () => {
    setRepoName('');
    setBranchName('');
    setPathToDir('');
    setUrl(null);
    setIsSubmitted(false);
  };

  return (
    // we're calling the styles object here to apply the styles to the elements using their class names
    <div className={styles['form-container']}>
      <div>
        <label for="github-repo">Repository URL</label>
        <small>
          Enter the URL of the GitHub repository containing the Hasura assets
        </small>
        <input
          type="text"
          id="github-repo"
          name="github-repo"
          value={repoName}
          onChange={e => handleChange(e)}
          placeholder="e.g. https://github.com/your-github-username/repository-name"
        />
      </div>
      <div>
        <label for="mms-path">Path to directory containing Hasura assets</label>
        <small>
          Enter the path to the directory containing your Metadata, Migrations
          and Seeds. If these are present in the root directory of the
          repository, you can skip this.
        </small>
        <input
          type="text"
          id="mms-path"
          name="mms-path"
          value={pathToDir}
          onChange={e => handleChange(e)}
          placeholder="e.g. hasura"
        />
      </div>
      <div>
        <label htmlFor="github-branch">Branch name</label>
        <small>
          Enter the repository branch the Hasura assets should be picked from.
          If you'd like to use the default branch of your repository, you can
          skip this.
        </small>
        <input
          type="text"
          id="github-branch"
          name="github-branch"
          value={branchName}
          onChange={e => handleChange(e)}
          placeholder="e.g. main"
        />
      </div>
      <div
        className={styles['code-block']}
        style={{ maxHeight: isSubmitted && `800px` }}
      >
        {isSubmitted && (
          <p>Awesome! We've copied this link to your clipboard for you 🎉</p>
        )}
        <code>{url}</code>
      </div>
      <div className={styles['button-container']}>
        <button onClick={() => handleClear()}>Clear form</button>
        <button
          onClick={() => handleSubmit()}
          disabled={!isSubmittable}
          style={{ cursor: isSubmittable ? 'pointer' : 'not-allowed' }}
          title={
            isSubmittable
              ? 'Click to submit'
              : 'Please fill out all required fields'
          }
        >
          Generate URL
        </button>
      </div>
    </div>
  );
};

export default CloudDeployURLGenerationForm;
