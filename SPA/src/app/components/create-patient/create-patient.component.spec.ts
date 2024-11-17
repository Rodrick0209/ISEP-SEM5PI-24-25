import { ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { CreatePatientComponent } from './create-patient.component';
import { PatientService } from '../../services/patient.service';
import { MessageService } from '../../services/message.service';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

describe('CreatePatientComponent', () => {
  let component: CreatePatientComponent;
  let fixture: ComponentFixture<CreatePatientComponent>;
  let patientService: jasmine.SpyObj<PatientService>;
  let messageService: jasmine.SpyObj<MessageService>;
  let router: jasmine.SpyObj<Router>;

  beforeEach(async () => {
    const patientServiceSpy = jasmine.createSpyObj('PatientService', ['createPatient']);
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['setMessage']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [CreatePatientComponent, FormsModule, CommonModule],
      providers: [
        { provide: PatientService, useValue: patientServiceSpy },
        { provide: MessageService, useValue: messageServiceSpy },
        { provide: Router, useValue: routerSpy }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(CreatePatientComponent);
    component = fixture.componentInstance;
    patientService = TestBed.inject(PatientService) as jasmine.SpyObj<PatientService>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should show confirmation modal', () => {
    component.confirmSubmission();
    expect(component.showConfirmation).toBeTrue();
  });

  it('should close confirmation modal', () => {
    component.closeConfirmationModal();
    expect(component.showConfirmation).toBeFalse();
  });

  it('should navigate to /patients on cancel', () => {
    component.onCancel();
    expect(router.navigate).toHaveBeenCalledWith(['/patients']);
  });

  it('should submit form and navigate to /patients on success', () => {
    const patientForm = { valid: true };
    component.submitForm.dateOfBirth = new Date().toISOString();
    const patientData = { ...component.submitForm, dateOfBirth: new Date(component.submitForm.dateOfBirth).toISOString() };
    patientService.createPatient.and.returnValue(of({}));

    component.onSubmit(patientForm);

    expect(patientService.createPatient).toHaveBeenCalledWith(
      patientData.firstName,
      patientData.lastName,
      patientData.fullName,
      patientData.dateOfBirth,
      patientData.gender,
      patientData.email,
      patientData.phoneNumber,
      patientData.street,
      patientData.postalCode,
      patientData.city,
      patientData.country,
      patientData.emergencyContactName,
      patientData.emergencyContactEmail,
      patientData.emergencyContactPhoneNumber
    );
    expect(messageService.setMessage).toHaveBeenCalledWith(`Patient ${patientData.fullName} successfully created!`);
    expect(router.navigate).toHaveBeenCalledWith(['/patients']);
  });


  it('should set error message on form submission failure', () => {
    const patientForm = { valid: true };
    component.submitForm.dateOfBirth = new Date().toISOString();
    const errorResponse = { error: { message: 'Error creating patient' } };
    patientService.createPatient.and.returnValue(throwError(errorResponse));

    component.onSubmit(patientForm);

    expect(component.errorMessage).toBe('Failed to create patient: Error creating patient');
  });

  it('should log form invalid message when form is invalid', () => {
    spyOn(console, 'log');
    const patientForm = { valid: false };

    component.onSubmit(patientForm);

    expect(console.log).toHaveBeenCalledWith('Form is invalid');
  });
});