FROM swipl
CMD ["bash"]

# Needed for AWS to properly handle UTF-8
ENV PYTHONIOENCODING=UTF-8

ENV PATH=$PATH:/root/.local/bin:/home/ag/.local/bin


# Install yum libraries, sudo command, Python, curses (for terminal display?)
# AWS utilities, make, and mustache (for variant processing).
# The alternate repos for yum packages may not be necessary any longer.
RUN apt update
RUN apt-get install -y --no-install-recommends \
        python3.5 \
        python3-pip \
        # make \
        rubygems

RUN gem install mustache

# Needed setuptools to make the pip installs on the subsequent line work.
# Else, I got "Command "python setup.py egg_info" failed with error code 1"
RUN python3 -m pip install setuptools
RUN python3 -m pip install awscli requests

# Still haven't done these:
#     && yum install -y sudo \
#     && yum install -y ncurses-libs \
#     && yum install -y make \

# ag = autograder, just a username for the account to run grading.
RUN useradd ag

# This is the required folder in which PrairieGrade places files.
RUN mkdir /grade


ADD main.py /
    

