/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.{html,js,cljs}"],
  theme: {
    extend: {},
  },
  plugins: [
    require('@tailwindcss/forms'),
  ],
    safelist: [
        {
            pattern: /col-span-+/,
            variants: ['sm'],
        }
    ],
}
