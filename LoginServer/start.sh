#!/bin/bash
erl +K true +P 1048576 +Q 1048576 +t 4194304 -name biganans_login@127.0.0.1 -setcookie biganans_login -pa ebin deps/ebin config config/app

