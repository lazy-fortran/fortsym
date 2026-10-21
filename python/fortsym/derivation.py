"""Bounded scalar recording through the native transcript owner.

Ordinary assertions, Python control flow and arbitrary direct C calls are outside
coverage. Use check_zero/check_unknown for explicit witnesses. Unsupported API
calls through the scalar result boundary poison the session and refuse export.
"""
import ctypes


class Derivation:
    def __init__(self, source_sha256, *, arena=None, max_steps=10000):
        from . import _default
        self.arena = arena if arena is not None else _default()
        self.source_sha256 = source_sha256
        self.max_steps = max_steps
        self.source = None
        self._active = False

    def _status(self, status, message):
        from . import FortSymError, _decode
        if status:
            raise FortSymError(status, _decode(message), 'derivation')

    def __enter__(self):
        from . import _message
        if self.arena._derivation is not None:
            raise RuntimeError('a derivation is already active in this arena')
        message = _message()
        lib = self.arena._lib
        status = lib.derivation_begin(self.arena._require(),
                                      self.source_sha256.encode(), self.max_steps,
                                      message, len(message))
        if status:
            lib.derivation_end(self.arena._require(), message, len(message))
            self._status(status, message)
        self.arena._derivation = self
        self._active = True
        return self

    def __exit__(self, exc_type, exc, traceback):
        from . import _message
        try:
            if exc_type is None:
                self.source = self.export_sympy()
        finally:
            self.arena._derivation = None
            self._active = False
            message = _message()
            self.arena._lib.derivation_end(self.arena._require(), message,
                                           len(message))

    def observe(self, operation, inputs, output, literal=''):
        from . import _CVOID, _message
        message = _message()
        handles = (_CVOID * len(inputs))(*inputs)
        steps = (ctypes.c_int * len(inputs))(*(self._step(h) for h in inputs))
        observed_step = ctypes.c_int()
        status = self.arena._lib.derivation_observe(
            self.arena._require(), operation.encode(), handles, steps, len(inputs),
            output._require(), literal.encode(), ctypes.byref(observed_step),
            message, len(message))
        self._status(status, message)
        output._handle._record_owner = self
        output._handle._record_step = observed_step.value

    def _step(self, handle):
        if getattr(handle, '_record_owner', None) is not self:
            self.refuse('unrecorded input or foreign session')
        return handle._record_step

    def result(self, function, arguments, output):
        operation = function.__name__.removeprefix('fortsym_')
        literal = ''
        inputs = arguments[1:]
        if operation == 'int':
            operation, literal, inputs = 'integer', str(arguments[1]), ()
        elif operation == 'rational':
            literal = f'{arguments[1]}/{arguments[2]}'
            inputs = ()
        elif operation in ('symbol', 'constant', 'exact'):
            literal, inputs = arguments[1].decode(), ()
        elif operation not in ('add', 'subtract', 'multiply', 'divide', 'power',
                                'differentiate', 'substitute', 'simplify',
                                'expand', 'cancel', 'factor'):
            self.refuse('unsupported operation: ' + operation)
        self.observe(operation, inputs, output, literal)

    def assume(self, expression, facts):
        from . import _message
        message = _message()
        status = self.arena._lib.derivation_assume(
            self.arena._require(), expression._require(), self._step(expression._handle), int(facts),
            message, len(message))
        self._status(status, message)

    def _directive(self, operation, expression, label):
        from . import _message
        message = _message()
        handle = expression._require() if expression is not None else None
        step = self._step(handle) if handle is not None else 0
        status = self.arena._lib.derivation_directive(
            self.arena._require(), operation.encode(), handle, step, label.encode(),
            message, len(message))
        self._status(status, message)
        return expression

    def name(self, label, expression):
        return self._directive('name', expression, label)

    def condition_nonzero(self, expression, label='excluded pole'):
        return self._directive('condition_nonzero', expression, label)

    def check_zero(self, expression, label='identity'):
        return self._directive('zero', expression, label)

    def check_unknown(self, expression, label='unresolved'):
        return self._directive('unknown', expression, label)

    def refuse(self, reason):
        self._directive('refuse', None, reason)

    def export_sympy(self):
        from . import _message, _SIZE
        if not self._active:
            if self.source is None:
                raise RuntimeError('derivation has no completed artifact')
            return self.source
        message = _message()
        required = _SIZE()
        lib = self.arena._lib
        # Native size query reports RESOURCE_LIMIT while returning the exact size.
        status = lib.derivation_export(self.arena._require(), None, 0,
                                       ctypes.byref(required), message, len(message))
        if status != 6:  # FORTSYM_RESOURCE_LIMIT
            self._status(status, message)
        buffer = ctypes.create_string_buffer(required.value)
        status = lib.derivation_export(self.arena._require(), buffer, len(buffer),
                                       ctypes.byref(required), message, len(message))
        self._status(status, message)
        return buffer.value.decode()
