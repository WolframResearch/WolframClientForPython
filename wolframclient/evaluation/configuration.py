from __future__ import absolute_import, print_function, unicode_literals

from configparser import SafeConfigParser, NoSectionError, NoOptionError
from wolframclient.evaluation.cloud.exceptions import ConfigurationException, ConfigurationWarning
from warnings import warn

__all__ = ['Configuration',
           'WolframPublicCloudParameters',
           'WolframPublicCloudConfig'
           ]

class Configuration(object):
    api_section = 'API'
    authentication_section = 'Authentication'

    sections = {
        api_section: ['api_endpoint'],
        authentication_section: [
            'request_token_endpoint',
            'access_token_endpoint',
            'consumer_key',
            'consumer_secret',
            'xauth_consumer_key',
            'xauth_consumer_secret']
    }

    optional_keys = frozenset(['xauth_consumer_key', 'xauth_consumer_secret'])

    def __init__(self):
        self._parser = None

    def _set_parser(self, parser):
        self._check_mandatory_sections(parser)
        self._parser = parser
        self._read_keys()

    def _check_mandatory_sections(self, parser):
        for section in (Configuration.authentication_section, Configuration.api_section):
            if not parser.has_section(section):
                raise ConfigurationException(
                    'Server configuration must contain section: %s' % section)

    def read(self, filenames):
        parser = SafeConfigParser()
        parser.read(filenames)
        self._set_parser(parser)

    def read_file(self, file):
        parser = SafeConfigParser()
        parser.read_file(file)
        self._set_parser(parser)

    def read_dict(self, dictionary):
        parser = SafeConfigParser()
        parser.read_dict(dictionary)
        self._set_parser(parser)

    def _read_keys(self):
        for section, keys in Configuration.sections.items():
            for key in keys:
                if key in Configuration.optional_keys:
                    value = self._parser.get(section, key, fallback=None)
                    if value is not None and value == "":
                        value = None
                else:
                    try:
                        value = self._parser.get(section, key)
                        self.warn_if_empty_option(key, value)
                    except NoOptionError as e:
                        raise ConfigurationException(
                            'Server configuration must contain option value: %s' % e.option)

                self.__setattr__(key, value)

    def warn_if_empty_option(self, key, option):
        if option == "":
            warn('Option %s found but is empty.' %
                 key, category=ConfigurationWarning)


# PRD
WolframPublicCloudParameters = {
    'API': {
        'api_endpoint': "https://www.wolframcloud.com"
    },
    'Authentication':{
        'request_token_endpoint' : 'https://account.wolfram.com/auth/request-token',
        'access_token_endpoint' : 'https://account.wolfram.com/auth/access-token',
        'consumer_key' : 'ytpbLSZCUz0q4OX+3NR9WHSUdKdnf2j/yg5Pk+MTRdo=',
        'consumer_secret' : 'W1ciz5KPhz5NCm26mAx4/EcT+lfnCTBqgLVEP+bY3B0=',
        'xauth_consumer_key' : 'tbd',
        'xauth_consumer_secret' : 'tbd'
    }
}
WolframPublicCloudConfig = Configuration()
WolframPublicCloudConfig.read_dict(WolframPublicCloudParameters)

