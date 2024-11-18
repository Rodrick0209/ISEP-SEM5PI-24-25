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
import { StaffsComponent } from './components/staffs/staffs.component';
import { StaffDetailsComponent } from './components/staff-details/staff-details.component';
import { CreateStaffComponent } from './components/create-staff/create-staff.component';
import { EditStaffComponent } from './components/edit-staff/edit-staff.component';
import { DeleteStaffComponent } from './components/delete-staff/delete-staff.component';
import { RegisterConfirmationComponent } from './components/register-confirmation/register-confirmation.component';
import { PlanningComponent } from './components/planning/planning.component';
import { EditOperationTypeComponent } from './components/edit-operation-type/edit-operation-type.component';
import { UserProfileComponent } from './components/user-profile/user-profile.component';
import { UserSettingsComponent } from './components/user-settings/user-settings.component';
import { EditUserComponent } from './components/edit-user/edit-user.component';
import { EditConfirmationComponent } from './components/edit-confirmation/edit-confirmation.component';
import { OperationRequestsComponent } from './components/operationRequests/operationRequests.component';
import { DeleteOperationRequestsComponent } from './components/delete-operation-requests/delete-operation-requests.component';
import { EditOperationRequestsComponent } from './components/edit-operation-requests/edit-operation-requests.component';
import { CreateOperationRequestsComponent } from './components/create-operation-requests/create-operation-requests.component';
import { ResetPasswordComponent } from './components/reset-password/reset-password.component';
import { ThreeMFLoader } from 'three/examples/jsm/Addons.js';
import { ThreeViewComponent } from './components/three-view/three-view.component';
import { DeleteUserComponent } from './components/delete-user/delete-user.component';
import { DeleteConfirmationComponent } from './components/delete-confirmation/delete-confirmation.component';



export const routes: Routes = [
    { path: '', redirectTo: 'home', pathMatch: 'full' },
    { path: 'home', component: HomeComponent },
    { path: 'about', component: AboutComponent },
    { path: 'contact', component: ContactComponent },
    { path: 'view', component: ThreeViewComponent },
    { path: 'login', component: LoginComponent },
    { path: 'register', component: RegisterComponent },
    { path: 'register/confirm', component: RegisterConfirmationComponent },
    { path: 'confirmation-error', component: ConfirmationErrorComponent },
    { path: 'confirmation-success', component: ConfirmationSuccessComponent },
    { path: 'reset-password', component: ResetPasswordComponent},
    {
        path: 'staffs',
        component: StaffsComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    {
        path: 'staff/details/:id',
        component: StaffDetailsComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    {
        path: 'staff/create',
        component: CreateStaffComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    {
        path: 'staff/edit/:id',
        component: EditStaffComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    {
        path: 'staff/delete/:id',
        component: DeleteStaffComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    { path: 'operationRequests', 
        component: OperationRequestsComponent, 
        canActivate: [AuthGuard],
        data: { role: 'doctor' }
    },
    {
        path: 'operationRequests/create',
        component: CreateOperationRequestsComponent,
        canActivate: [AuthGuard],
        data: { role: 'doctor' }
    },
    { path : 'operationRequests/delete/:id',
        component: DeleteOperationRequestsComponent,
        canActivate: [AuthGuard],
        data: { role: 'doctor' }  
    },
    
    { path : 'operationRequests/edit/:id',
        component: EditOperationRequestsComponent, 
        canActivate: [AuthGuard],
        data: { role: 'doctor' }
    }, 
        
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
    },
    {
        path: 'operation-types/edit/:id', // Add this route
        component: EditOperationTypeComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    {
        path: 'profile/:email',
        component: UserProfileComponent,
        canActivate: [AuthGuard],
        data: { role: 'patient' }
    },
    {
        path: 'settings/:email',
        component: UserSettingsComponent,
        canActivate: [AuthGuard],
        data: { role: 'patient' }
    },
    {
        path: 'edit/confirm',
        component: EditConfirmationComponent,
        canActivate: [AuthGuard],
        data: { role: 'patient' }
    },
    {
        path: 'edit/:email',
        component: EditUserComponent,
        canActivate: [AuthGuard],
        data: { role: 'patient' }
    },
    {
        path: 'view',
        component: ThreeViewComponent
    },
    {
        path: 'delete/confirm',
        component: DeleteConfirmationComponent,
        canActivate: [AuthGuard],
        data: {role: 'patient'}
    },
    {
        path: 'delete/:email',
        component: DeleteUserComponent,
        canActivate: [AuthGuard],
        data: {role: 'patient'}
    }
];
