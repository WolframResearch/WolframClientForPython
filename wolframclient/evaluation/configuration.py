from __future__ import absolute_import, print_function, unicode_literals

from configparser import ConfigParser, NoSectionError, NoOptionError
from wolframclient.evaluation.cloud.exceptions import ConfigurationException, ConfigurationWarning
from warnings import warn
import logging

logger = logging.getLogger(__name__)

__all__ = ['Configuration', 'user_credential_configuration', 'server_configuration',
           'WolframPublicCloudParameters', 'WolframPublicCloudConfig'
           ]

class Configuration(object):
    ''' A structured representation of a configuration file.

    This class represents a configuration file structured in sections.
    Each sections can have pairs of key-value. The configuration is initialized
    with a scheme: a dictionary associating string keys, the section names, to list of
    parameter names as strings.
    The configuration is then populated using one of the `read` methods.
    Some sections can be optional in which case no error is raise if missing from the
    source. Similarly some parameters can be optional in a mandatory section.
    This class allows a fine tune of which values from a given data source are relevant
    for the purpose of a given configuration.
    
    Once populated, the configuration has new attributes named using:
    `{section}_{parameter}` section and parameter in lower case.

    Examples of predefined schemes can be found at the end of this module, they define 
    server configuration (`server_configuration`), user credentials
    (`user_credential_configuration`), and secured key (`sak_configuration`).

    The value `WolframPublicCloudConfig` contains the populated configuration of the
    Wolfram public cloud.
    '''
    def __init__(self, sections, optional_sections={}, optional_keys={}):
        self.log_values = True
        self.sections = {}
        for section, params in sections.items():
            self.sections[section] = frozenset(params)
        self.optional_sections = frozenset(optional_sections)
        for section in self.optional_sections:
            if section not in sections.keys():
                raise ConfigurationException(
                    'Optional section %s not declared as section.' % section)
        self.optional_keys = {}
        for section, params in optional_keys.items():
            if section not in self.sections:
                raise ConfigurationException('Section %s not declared as a section cannot have optional keys.' % section)
            if section in self.optional_sections:
                raise ConfigurationWarning("Section %s is optional, it's parameters are de facto optional." % section)
            declared_params = self.sections.get(section)
            for param in params:
                if param not in declared_params:
                    raise ConfigurationException('Optional parameter {} not declared for section {}.'.format(param, section))
            self.optional_keys[section] = frozenset(params)

        self._parser = None

    def _set_parser(self, parser):
        self._check_mandatory_sections(parser)
        self._parser = parser
        self._read_keys()

    def get_optional_keys_of_section(self, section):
        return self.optional_keys.get(section, frozenset())

    def _check_mandatory_sections(self, parser):
        for section in self.sections:
            if not parser.has_section(section):
                raise ConfigurationException(
                    'Server configuration must contain section: %s' % section)

    def read(self, filenames):
        ''' Populate the configuration with values gathered from a list of files defined by they filenames'''
        if logger.level <= logging.DEBUG:
            from wolframclient.utils.functional import riffle
            if isinstance(filenames, list):
                out_filenames = "".join(riffle(filenames, ', '))
                logger.debug('Configuration read from files: %s', out_filenames)
            else:
                logger.debug('Configuration read from %s', filenames)
        parser = ConfigParser()
        parser.read(filenames)
        self._set_parser(parser)
        return self

    def read_file(self, file):
        ''' Populate the configuration with values gathered from a file-like object'''
        if logger.level <= logging.DEBUG:
            logger.debug('Configuration read from %s', file.name)
        parser = ConfigParser()
        parser.read_file(file)
        self._set_parser(parser)
        return self

    def read_dict(self, dictionary):
        ''' Populate the configuration with values gathered from a `dict`'''
        logger.debug('Configuration read from dictionary.')
        parser = ConfigParser()
        parser.read_dict(dictionary)
        self._set_parser(parser)
        return self

    def _read_keys(self):
        for section, keys in self.sections.items():
            logger.debug('[%s]', section)
            section_optional_params = self.get_optional_keys_of_section(section)
            for key in keys:
                if section in self.optional_sections or key in section_optional_params:
                    value = self._parser.get(section, key, fallback=None)
                    if value is not None and value == "":
                        value = None
                else:
                    try:
                        value = self._parser.get(section, key)
                        self.warn_if_empty_option(key, value)
                    except NoOptionError as e:
                        raise ConfigurationException(
                            'Expecting configuration to contain option value: %s' % e.option)
                name = '{}_{}'.format(section.lower(), key.lower())
                if self.log_values:
                    logger.debug('\t%s=%s', name, value)
                else:
                    logger.debug('\t%s= *not logged*', name)
                self.__setattr__(name, value)

    def warn_if_empty_option(self, key, option):
        if option == "":
            warn('Option %s found but is empty.' %
                 key, category=ConfigurationWarning)


def user_credential_configuration():
    ''' Return an initialized instance of `Configuration` specifying user credentials '''
    config = Configuration(
        {'User': ['id', 'password']}
    )
    config.log_values = False
    return config


def sak_configuration():
    ''' Return an initialized instance of `Configuration` specifying a secured authenticated key '''
    config = Configuration(
        {'SAK': ['consumer_key', 'consumer_secret']}
    )
    config.log_values = False
    return config

def server_configuration():
    ''' Return an initialized instance of `Configuration` specifying a server. '''
    return Configuration(
        {
            'API': ['api_endpoint'],
            'Authentication': [
                'request_token_endpoint',
                'access_token_endpoint',
                'xauth_consumer_key',
                'xauth_consumer_secret']
        }, 
        optional_keys={
            'Authentication': ['xauth_consumer_key', 'xauth_consumer_secret']
        }
    )

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
WolframPublicCloudConfig = server_configuration().read_dict(WolframPublicCloudParameters)

