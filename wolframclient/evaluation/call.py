# # -*- coding: utf-8 -*-

# from __future__ import absolute_import, print_function, unicode_literals
# from wolframclient.utils.six import string_types
# from wolframclient.utils.encoding import force_text
# from wolframclient.evaluation.cloud.exceptions import InputException
# class WolframCall(object):
#     def perform(self, input):
#         raise NotImplementedError()


# class WolframAPICall(WolframCall):
#     def __init__(self, url, session, input_encoders=None, decoder=force_text):
#         self.cloud_context = context
#         self.url = url
#         self.input_encoders = input_encoders
#         self.decoder = decoder
#         self.result = None

#     def perform(self, input_parameters={}):
#         self.result = self.cloud_context.call(
#             self.url, input_parameters, input_encoders=self.input_encoders, decoder=self.decoder)
#         return self.result
        
#     @property
#     def success(self):
#         return self.result is not None and self.result.success
