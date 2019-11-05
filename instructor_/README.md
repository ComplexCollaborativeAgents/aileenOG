# aileen-instructor

Script that generates lesson plans to teach aileen-agent

## Contributing

1. Clone the repository including the submodule:
   ```bash
   git clone --recurse-submodules git@gitlab-external.parc.com:aileen/aileen-instructor.git
   ```
   
1. Set up a Conda environment with dependencies:
   ```bash
   conda env create -f environment.yml
   ```
   
2. Activate the environment:
   ```bash
   conda activate aileen-instructor
   ```

3. Run the instructor:
   ```bash
   (aileen-instructor) python -m instructor 
   ```
   
4. Run the tests:
   ```bash
   (aileen-instructor) pytest
   ```
   
### PyCharm

1. Go to "File", "Settings...", "Tools", "Python Integrated Tools".
2. Under "Testing", set the default test runner to "pytest".
