import { Routes } from '@angular/router';
import { CubeComponent } from './components/cube/cube.component';
import { LoginComponent } from './login/login.component';
import { HomeComponent } from './components/home/home.component';
import { AboutComponent } from './components/about/about.component';
import { ContactComponent } from './components/contact/contact.component';
import { RegisterComponent } from './components/register/register.component';
import { ConfirmationErrorComponent } from './components/confirmation-error/confirmation-error.component';
import { ConfirmationSuccessComponent } from './components/confirmation-success/confirmation-success.component';

export const routes: Routes = [
    { path: '', redirectTo: 'home', pathMatch: 'full' },
    { path: 'home', component: HomeComponent },
    { path: 'about', component: AboutComponent },
    { path: 'contact', component: ContactComponent },
    { path: 'cube', component: CubeComponent },
    { path: 'login', component: LoginComponent },
    { path: 'register', component: RegisterComponent },
    { path: 'confirmation-error', component: ConfirmationErrorComponent },
    { path: 'confirmation-success', component: ConfirmationSuccessComponent }
];
