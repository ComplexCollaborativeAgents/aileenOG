from setuptools import setup, find_packages

setup(
    name='aileen',
    version='0.0.1',
    packages=find_packages(exclude=['strands_qsr_lib', 'vision']),
    install_requires=['coloredlogs', 'shapely'],
    include_package_data=True,
)
