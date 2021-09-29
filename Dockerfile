FROM swipl

# Will want to change this to, presumably, run main.py eventually.
#CMD ["bash"]

# Needed for AWS to properly handle UTF-8
ENV PYTHONIOENCODING=UTF-8

ENV PATH=$PATH:/root/.local/bin:/home/ag/.local/bin



# Needed to get pip. Maybe there's something better to do than a blanket update, however?
RUN apt update

# Install Python, AWS utilities, and mustache (for variant processing).
RUN apt-get install -y --no-install-recommends sudo
RUN apt-get install -y --no-install-recommends python3.5
RUN apt-get install -y --no-install-recommends python3-pip
RUN apt-get install -y --no-install-recommends rubygems

RUN gem install mustache

# Needed setuptools to make the pip installs on the subsequent line work.
# Else, I got "Command "python setup.py egg_info" failed with error code 1"
RUN python3 -m pip install setuptools
RUN python3 -m pip install awscli requests

# Mattox was installing ncurses-libs and make. Do I need those?

# ag = autograder, just a username for the account to run grading.
RUN useradd ag

# This is the required folder in which PrairieGrade places files.
RUN mkdir /grade


ADD main.py /
    

