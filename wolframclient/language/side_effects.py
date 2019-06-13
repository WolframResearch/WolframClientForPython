from __future__ import absolute_import, print_function, unicode_literals

import logging

from wolframclient.language import wl
from wolframclient.language.expression import WLExpressionMeta
from wolframclient.utils.functional import riffle

side_effect_logger = logging.getLogger("wolframclient.side_effect")

# the side effect logger is used by ExternalEvaluate to send side effects to the kernel.


def wl_print(*payload):
    return wl_side_effect(wl.Print(wl.Row(riffle(payload, " "))))


def wl_side_effect(payload):
    if not isinstance(payload, WLExpressionMeta):
        raise ValueError("Only expressions can create side_effects in wl")

    side_effect_logger.warning(payload)
