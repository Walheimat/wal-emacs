module.exports = {
	rules: {
		'header-max-length': [2, 'always', 72], // GitHub's limit, GitLab's is 1 KiB
		'type-empty': [2, 'never'],
		'type-case': [1, 'always', ['lower-case']],
		'scope-case': [2, 'always', ['lower-case', 'kebab-case']],
		'scope-empty': [1, 'never'],
		'subject-empty': [2, 'never'],
		'subject-case': [2, 'always', ['lower-case']],
		'subject-full-stop': [2, 'never'],
		'body-empty': [1, 'never'],
		'body-leading-blank': [2, 'always'],
		'body-case': [2, 'always', ['sentence-case']],
		'body-full-stop': [2, 'always', '.'],
		'body-max-line-length': [2, 'always', 70],
		'footer-max-line-length': [2, 'always', 70],
	},
	ignores: [(commit) => commit.includes('fixup')],
};
