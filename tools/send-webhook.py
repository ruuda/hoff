#!/bin/env python3

# Copyright 2016 Ruud van Asseldonk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3. See
# the licence file in the root of the repository.

from json import dumps
from sys import argv
from urllib.error import HTTPError
from urllib.request import Request, urlopen


def send_webhook(event_name, payload):
    url = 'http://localhost:5261/hook/github'
    headers = {'X-GitHub-Event': event_name}
    payload_bytes = dumps(payload).encode('utf8')
    request = Request(url, payload_bytes, headers, method='POST')

    try:
        response = urlopen(request)
    except HTTPError as error:
        response = error

    print('{}: {}'.format(response.code, response.read().decode('utf8')))


def main():
    """
    usage: tools/send-webhook.py <event_name> [<args>]
    events:
      pull_request <action> <number> <sha>
      issue_comment <action> <number> <body>
    """
    if argv[1] == 'pull_request':
        action = argv[2]
        number = int(argv[3])
        sha = argv[4]
        payload = {
            'action': action,
            'pull_request': {
                'base': {
                    'repo': {
                        'owner': {'login': 'baxterthehacker'},
                        'name': 'public-repo'
                    }
                },
                'number': number,
                'head': {'sha': sha},
                'user': {'login': 'johnthenitter'}
            }
        }
        send_webhook('pull_request', payload)

    if argv[1] == 'issue_comment':
        action = argv[2]
        number = int(argv[3])
        body = argv[4]
        payload = {
            'action': action,
            'repository': {
                'owner': {'login': 'baxterthehacker'},
                'name': 'public-repo'
            },
            'issue': {'number': number},
            'sender': {'login': 'johnthenitter'},
            'comment': {'body': body}
        }
        send_webhook('issue_comment', payload)


main()
