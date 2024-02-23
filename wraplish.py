#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import re
import queue
import threading
import traceback
import sys
from functools import wraps
from epc.server import ThreadingEPCServer
from utils import *

def threaded(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        thread = threading.Thread(target=func, args=args, kwargs=kwargs)
        thread.start()
        if hasattr(args[0], 'thread_queue'):
            args[0].thread_queue.append(thread)
    return wrapper

class Wraplish:
    def __init__(self, args):
        # Init EPC client port.
        init_epc_client(int(args[0]))

        # Build EPC server.
        self.server = ThreadingEPCServer(('127.0.0.1', 0), log_traceback=True)
        # self.server.logger.setLevel(logging.DEBUG)
        self.server.allow_reuse_address = True

        # ch = logging.FileHandler(filename=os.path.join(wraplish_config_dir, 'epc_log.txt'), mode='w')
        # formatter = logging.Formatter('%(asctime)s | %(levelname)-8s | %(lineno)04d | %(message)s')
        # ch.setFormatter(formatter)
        # ch.setLevel(logging.DEBUG)
        # self.server.logger.addHandler(ch)
        # self.server.logger = logger

        self.server.register_instance(self)  # register instance functions let elisp side call

        # Start EPC server with sub-thread, avoid block Qt main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()

        # Init.
        self.thread_queue = []
        self.buffer_content_dict = {}
        self.buffer_ticker_dict = {}

        # All Emacs request running in event_loop.
        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        # All LSP server response running in message_thread.
        self.message_queue = queue.Queue()
        self.message_thread = threading.Thread(target=self.message_dispatcher)
        self.message_thread.start()

        # Pass epc port and webengine codec information to Emacs when first start wraplish.
        eval_in_emacs('wraplish--first-start', self.server.server_address[1])

        # event_loop never exit, simulation event loop.
        self.event_loop.join()

    def event_dispatcher(self):
        try:
            while True:
                message = self.event_queue.get(True)

                if message["name"] == "open_file":
                    self._open_file(message["content"])
                elif message["name"] == "close_file":
                    self._close_file(message["content"])
                elif message["name"] == "action_func":
                    (func_name, func_args) = message["content"]
                    getattr(self, func_name)(*func_args)

                self.event_queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def message_dispatcher(self):
        try:
            while True:
                message = self.message_queue.get(True)
                if message["name"] == "server_process_exit":
                    self.handle_server_process_exit(message["content"])
                else:
                    logger.error("Unhandled wraplish message: %s" % message)

                self.message_queue.task_done()
        except:
            logger.error(traceback.format_exc())

    @threaded
    def change_buffer(self, buffer_name, begin, end, content, ticker, sync_flag):
        if sync_flag:
            self.sync_buffer(buffer_name)
        elif buffer_name not in self.buffer_content_dict:
            self.sync_buffer(buffer_name)
        else:
            buffer_content = self.buffer_content_dict[buffer_name]

            begin = epc_arg_transformer(begin)
            end = epc_arg_transformer(end)

            start_line = begin['line']
            start_char = begin['character']
            end_line = end['line']
            end_char = end['character']

            start_pos = get_position(buffer_content, start_line, start_char)
            end_pos = get_position(buffer_content, end_line, end_char)

            buffer_content = buffer_content[:start_pos] + content + buffer_content[end_pos:]

            self.buffer_content_dict[buffer_name] = buffer_content

        self.buffer_ticker_dict[buffer_name] = ticker
        self.find_space_positions(buffer_name, self.buffer_content_dict[buffer_name], ticker)

    def sync_buffer(self, buffer_name):
        self.buffer_content_dict[buffer_name] = get_emacs_func_result('get-buffer-content', buffer_name)

    def close_buffer(self, buffer_name):
        if buffer_name in self.buffer_content_dict:
            del self.buffer_content_dict[buffer_name]

    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()

    @threaded
    def find_space_positions(self, buffer_name, text, ticker):
        space_positions = []

        (self.add_space_after_chinese_punctuation,
         self.add_space_before_markdown_link) = get_emacs_vars([
             "wraplish-add-space-after-chinese-punctuation",
             "wraplish-add-space-before-markdown-link"
         ])

        # Find positions between English words and Chinese characters or Japanese Kanji
        for match in re.finditer(r'([a-zA-Z])([\u4e00-\u9fff])', text):
            space_positions.append(match.start(2))

        # Find positions between Chinese characters or Japanese Kanji and English words
        for match in re.finditer(r'([\u4e00-\u9fff])([a-zA-Z])', text):
            space_positions.append(match.start(2))

        # Find positions between English words and Korean Hangul characters
        for match in re.finditer(r'([a-zA-Z])([\uac00-\ud7a3])', text):
            space_positions.append(match.start(2))

        # Find positions between Korean Hangul characters and English words
        for match in re.finditer(r'((?<=\uac00)[\ud7a3])([a-zA-Z])', text):
            space_positions.append(match.start(2))

        # Find positions between Arabic numbers and Chinese characters or Japanese Kanji
        for match in re.finditer(r'([0-9])([\u4e00-\u9fff])', text):
            space_positions.append(match.start(2))

        # Find positions between Chinese characters or Japanese Kanji and Arabic numbers
        for match in re.finditer(r'([\u4e00-\u9fff])([0-9])', text):
            space_positions.append(match.start(2))

        # Find positions between Arabic numbers and Korean Hangul characters
        for match in re.finditer(r'([0-9])([\uac00-\ud7a3])', text):
            space_positions.append(match.start(2))

        # Find positions between Korean Hangul characters and Arabic numbers
        for match in re.finditer(r'((?<=\uac00)[\ud7a3])([0-9])', text):
            space_positions.append(match.start(2))

        # Find positions between percentages and Chinese characters
        for match in re.finditer(r'([0-9]+(?:\.[0-9]+)?)%(?=[\u4e00-\u9fff])', text):
            space_positions.append(match.end(0))

        # Find positions where a Chinese punctuation is not followed by a space or another Chinese punctuation, or follow \\
        if self.add_space_after_chinese_punctuation:
            chinese_punctuations = r'，|。|；|：|？|！|、'
            for match in re.finditer(r'({})(?!(\)|）|\*|[\s\“\”\"{}]|(?<=：)\\))'.format(chinese_punctuations, chinese_punctuations), text):
                space_positions.append(match.end(0))

        if self.add_space_before_markdown_link:
            # Find positions where a Unicode character is followed by a
            # Markdown link with link_text starting with an English letter
            for match in re.finditer(r'([\u4e00-\u9fff\uac00-\ud7a3])\[(?P<link_text>[a-zA-Z][^\]]+)]\((?P<url>[^\)]+)\)', text):
                space_positions.append(match.start(1) + 1)

            # Find positions where the closing parenthesis of a Markdown link
            # is followed by a Unicode character and not a space
            for match in re.finditer(r'\[(?P<link_text>[^\]]+)]\((?P<url>[^\)]*[a-zA-Z]+)\)([\u4e00-\u9fff\uac00-\ud7a3])(?!\s)', text):
                space_positions.append(match.start(3))

        space_positions.sort()

        if ticker == self.buffer_ticker_dict[buffer_name] and len(space_positions) > 0:
            eval_in_emacs("wraplish-insert-spaces", buffer_name, space_positions)

        return space_positions

if __name__ == "__main__":
    if len(sys.argv) >= 3:
        import cProfile
        profiler = cProfile.Profile()
        profiler.run("Wraplish(sys.argv[1:])")
    else:
        Wraplish(sys.argv[1:])
