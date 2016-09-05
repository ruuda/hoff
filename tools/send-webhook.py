#!/bin/env python3

# Hoff -- A gatekeeper for your commits
# Copyright 2016 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from json import dumps
from sys import argv
from urllib.error import HTTPError
from urllib.request import Request, urlopen


def send_webhook(event_name: str, payload: dict):
    url = 'http://localhost:5261/hook/github'
    headers = {'X-GitHub-Event': event_name}
    payload_bytes = dumps(payload).encode('utf8')
    request = Request(url, payload_bytes, headers, method='POST')

    try:
        response = urlopen(request)
    except HTTPError as error:
        response = error

    print('{}: {}'.format(response.code, response.read().decode('utf8')))


def send_pull_request_hook(action: str, number: int, sha: str):
    assert action in ('opened', 'closed', 'reopened', 'synchronize')
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


def send_issue_comment_hook(action: str, number: int, author: str, body: str):
    assert action in ('created', 'edited', 'deleted')
    payload = {
        'action': action,
        'repository': {
            'owner': {'login': 'baxterthehacker'},
            'name': 'public-repo'
        },
        'issue': {'number': number},
        'sender': {'login': author},
        'comment': {'body': body}
    }
    send_webhook('issue_comment', payload)


def main():
    """
    usage: tools/send-webhook.py <event_name> [<args>]
    events:
      pull_request <action> <number> <sha>
      issue_comment <action> <number> <author> <body>
    """
    if argv[1] == 'pull_request':
        action = argv[2]
        number = int(argv[3])
        sha = argv[4]
        send_pull_request_hook(action, number, sha)
    if argv[1] == 'issue_comment':
        action = argv[2]
        number = int(argv[3])
        author = argv[4]
        body = argv[5]
        send_issue_comment_hook(action, number, author, body)

main()
