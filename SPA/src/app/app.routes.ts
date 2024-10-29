import { Routes } from '@angular/router';
import { CubeComponent } from './cube/cube.component';
import { LoginComponent } from './login/login.component';

export const routes: Routes = [
    { path: 'cube', component: CubeComponent },
    { path: 'login', component: LoginComponent },
];
