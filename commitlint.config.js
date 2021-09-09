module.exports = {
	rules: {
		'header-max-length': [2, 'always', 72],
		'type-enum': [2, 'always', ['Fix', 'Feature', 'Style', 'Refactor', 'Improvement']],
		'type-case': [2, 'always', ['start-case']],
		'type-empty': [1, 'never'],
		'subject-case': [2, 'always', ['sentence-case']],
		'subject-empty': [2, 'never'],
		'body-empty': [2, 'never'],
		'body-case': [1, 'always', ['lower-case', 'sentence-case']],
		'body-full-stop': [1, 'always', '.'],
		'body-max-line-length': [2, 'always', 70],
		'footer-max-line-length': [2, 'always', 70],
	}
};
