import { Routes } from '@angular/router';
import { CubeComponent } from './components/cube/cube.component';
import { LoginComponent } from './components/login/login.component';
import { HomeComponent } from './components/home/home.component';
import { AboutComponent } from './components/about/about.component';
import { ContactComponent } from './components/contact/contact.component';
import { RegisterComponent } from './components/register/register.component';
import { ConfirmationErrorComponent } from './components/confirmation-error/confirmation-error.component';
import { ConfirmationSuccessComponent } from './components/confirmation-success/confirmation-success.component';
import { OperationTypeComponent } from './components/operation-types/operation-types.component';
import { AuthGuard } from './auth.guard';
import { PatientsComponent } from './components/patients/patients.component';
import { PatientDetailsComponent } from './components/patient-details/patient-details.component';
import { CreatePatientComponent } from './components/create-patient/create-patient.component';
import { EditPatientComponent } from './components/edit-patient/edit-patient.component';
import { DeletePatientComponent } from './components/delete-patient/delete-patient.component';
import { RegisterConfirmationComponent } from './components/register-confirmation/register-confirmation.component';
import { PlanningComponent } from './components/planning/planning.component';

export const routes: Routes = [
    { path: '', redirectTo: 'home', pathMatch: 'full' },
    { path: 'home', component: HomeComponent },
    { path: 'about', component: AboutComponent },
    { path: 'contact', component: ContactComponent },
    { path: 'cube', component: CubeComponent },
    { path: 'planning', component: PlanningComponent},   
    { path: 'login', component: LoginComponent },
    { path: 'register', component: RegisterComponent },
    { path: 'register/confirm', component: RegisterConfirmationComponent },
    { path: 'confirmation-error', component: ConfirmationErrorComponent },
    { path: 'confirmation-success', component: ConfirmationSuccessComponent },
    { 
        path: 'operationType',
        component: OperationTypeComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    {
        path: 'patients',
        component: PatientsComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    {
        path: 'patient/details/:medicalRecordNumber',
        component: PatientDetailsComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    {
        path: 'patient/create',
        component: CreatePatientComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    {
        path: 'patient/edit/:medicalRecordNumber',
        component: EditPatientComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    {
        path: 'patient/delete/:medicalRecordNumber',
        component: DeletePatientComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    }
];
