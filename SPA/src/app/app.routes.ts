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
import { ScheduleResultsComponent } from './components/schedule-results/schedule-results.component';
import { SpecializationsComponent } from './components/specializations/specializations.component';
import { CreateSpecializationComponent } from './components/create-specialization/create-specialization.component';
import { AddRoomTypeComponent } from './components/add-room-type/add-room-type.component';
import { EditSpecializationComponent } from './components/edit-specialization/edit-specialization.component';
import { DeleteSpecializationComponent } from './components/delete-specialization/delete-specialization.component';
import { AllergiesCatalogComponent } from './components/allergies-catalog/allergies-catalog.component';
import { CreateAllergiesCatalogItemComponent } from './components/create-allergies-catalog-item/create-allergies-catalog-item.component';
import { MedicalConditionsComponent } from './components/medical-conditions/medical-conditions.component';
import { CreateMedicalConditionsComponent } from './components/create-medical-conditions/create-medical-conditions.component';
import { MedicalRecordEntriesComponent } from './components/medical-record-entries/medical-record-entries.component';
import { CreateAppointmentComponent } from './components/create-appointment/create-appointment.component';
import { EditAppointmentComponent } from './components/edit-appointment/edit-appointment.component';
import { AppointmentsComponent } from './components/appointments/appointments.component';
import { EditMedicalRecordComponent } from './components/edit-medical-record/edit-medical-record.component';
import { EditAllergyComponent } from './components/edit-allergy/edit-allergy.component';
import { EditMedicalConditionComponent } from './components/edit-medical-condition/edit-medical-condition.component';
import { DownloadMedicalHistoryComponent } from './components/download-medical-history/download-medical-history.component';
import { RequestDeletePatientDataComponent } from './components/request-delete-patient-data/request-delete-patient-data.component';
import { GeneticAlgorithmComponent } from './components/genetic-algorithm/genetic-algorithm.component';
import { PrivacyPolicyComponent } from './components/privacy-policy/privacy-policy.component';


export const routes: Routes = [
    { path: '', redirectTo: 'home', pathMatch: 'full' },
    { path: 'home', component: HomeComponent },
    { path: 'about', component: AboutComponent },
    { path: 'contact', component: ContactComponent },
    { path: 'privacy-policy', component: PrivacyPolicyComponent },
    {   path: 'view',
        component: ThreeViewComponent,
        canActivate: [AuthGuard],
        data: { roles: ['admin', 'doctor'] }
    },
    { path: 'login', component: LoginComponent },
    { path: 'register', component: RegisterComponent },
    { path: 'register/confirm', component: RegisterConfirmationComponent },
    { path: 'confirmation-error', component: ConfirmationErrorComponent },
    { path: 'confirmation-success', component: ConfirmationSuccessComponent },
    { path: 'reset-password', component: ResetPasswordComponent},
    { path: 'specializations',
        component: SpecializationsComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    {
        path: 'specialization/edit/:id/:name',
        component: EditSpecializationComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }

    },
    {
        path: 'planning/genetic-algorithm',
        component: GeneticAlgorithmComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    {
      path: 'specialization/create',
      component: CreateSpecializationComponent,
      canActivate: [AuthGuard],
      data: { role: 'admin' }
    },
    {
        path: 'specialization/delete/:id',
        component: DeleteSpecializationComponent,
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    { path: 'planning', 
        component:PlanningComponent, 
        canActivate: [AuthGuard],
        data: { role: 'admin' }
    },
    { path: 'planningResults',
        component: ScheduleResultsComponent ,
        canActivate : [AuthGuard],
        data: { role: 'admin' }
    },
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
        data: { roles: ['admin', 'doctor'] }
    },
    {
        path: 'patient/details/:medicalRecordNumber',
        component: PatientDetailsComponent,
        canActivate: [AuthGuard],
        data: { roles: ['admin', 'doctor', 'patient'] }
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
        path: 'edit-medical-record/:medicalRecordNumber',
        component: EditMedicalRecordComponent,
        canActivate: [AuthGuard],
        data: { role: 'doctor' }
        
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
    },
    {
        path: 'room-types/add',
        component: AddRoomTypeComponent,
        canActivate: [AuthGuard],
        data: {role: 'admin'}
    },
    {
        path: 'allergiesCatalog',
        component: AllergiesCatalogComponent,
        canActivate: [AuthGuard],
        data: {role: 'admin'}
    },
    {
        path: 'allergiesCatalog/add',
        component: CreateAllergiesCatalogItemComponent,
        canActivate: [AuthGuard],
        data: {role: 'admin'}
    },
    {
        path: 'medicalConditions',
        component: MedicalConditionsComponent,
        canActivate: [AuthGuard],
        data: {role: 'admin'}
    },
    {
        path: 'medicalConditions/add',
        component: CreateMedicalConditionsComponent,
        canActivate: [AuthGuard],
        data: {role: 'admin'}
    },
    {
        path: 'patient/medical-record/:medicalRecordNumber',
        component: MedicalRecordEntriesComponent,
        canActivate: [AuthGuard],
        data: {role: 'doctor'}
    },
    {
        path: 'appointment/create',
        component: CreateAppointmentComponent,
        canActivate: [AuthGuard],
        data: {role: 'doctor'}
    },
    {
        path: 'appointment/edit/:id',
        component: EditAppointmentComponent,
        canActivate: [AuthGuard],
        data: {role: 'doctor'}
    },
    {
        path: 'appointments',
        component: AppointmentsComponent,
        canActivate: [AuthGuard],
        data: {role: 'doctor'}
    },
    {
        path: 'allergiesCatalog/edit/:allergyName',
        component: EditAllergyComponent,
        canActivate: [AuthGuard],
        data: {role: 'admin'}
    },
    {
        path: 'medicalConditions/edit/:medicalConditionName',
        component: EditMedicalConditionComponent,
        canActivate: [AuthGuard],
        data: {role: 'admin'}
    },
    {
        path: 'download-medical-history/:email',
        component: DownloadMedicalHistoryComponent,
        canActivate: [AuthGuard],
        data: {role: 'patient'}
    },
    {
        path: 'request-delete/:email',
        component: RequestDeletePatientDataComponent,
        canActivate: [AuthGuard],
        data: {role: 'patient'}
    }

];
