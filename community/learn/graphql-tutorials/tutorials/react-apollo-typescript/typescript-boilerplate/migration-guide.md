change all jsx files to .tsx
keep all js files to .ts

tsconfig.json - "noImplicitAny": false, [Change this later to true and fix types]

install @types/module-name for each module (like auth0-js)

const style = {
    'width': '100px';
} as any;

Changing <Button onClick={handleLogin} to <Button onClick={() => handleLogin()} 

why use arrow function?


https://decembersoft.com/posts/error-ts2532-optional-react-component-props-in-typescript/





https://www.logicbig.com/tutorials/misc/typescript/interface-describing-function.html