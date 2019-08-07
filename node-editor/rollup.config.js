import builtins   from 'rollup-plugin-node-builtins';
import resolve    from 'rollup-plugin-node-resolve';
import cjs        from 'rollup-plugin-commonjs';

export default {
    treeshake: false,
    input: './js/imports.js',
    output: {
        strict: false,
        interop: false,
	file: './js/bundle.js',
	sourcemap: false,
	format: 'cjs'
    },
    plugins: [
        builtins({preferBuiltins: false}),
	resolve(), // tells Rollup how to find date-fns in node_modules
        cjs(),
    ]
};
