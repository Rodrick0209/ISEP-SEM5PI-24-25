import { ComponentFixture, TestBed } from '@angular/core/testing';
import { EditPatientComponent } from './edit-patient.component';
import { PatientService } from '../../services/patient.service';
import { MessageService } from '../../services/message.service';
import { ActivatedRoute, Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

describe('EditPatientComponent', () => {
  let component: EditPatientComponent;
  let fixture: ComponentFixture<EditPatientComponent>;
  let patientService: jasmine.SpyObj<PatientService>;
  let messageService: jasmine.SpyObj<MessageService>;
  let router: jasmine.SpyObj<Router>;
  let route: ActivatedRoute;

  beforeEach(async () => {
    const patientServiceSpy = jasmine.createSpyObj('PatientService', {
      getPatientByMedicalRecordNumber: of({}),
      editPatient: of({})
    });
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['setMessage']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [FormsModule, CommonModule],
      providers: [
        { provide: PatientService, useValue: patientServiceSpy },
        { provide: MessageService, useValue: messageServiceSpy },
        { provide: Router, useValue: routerSpy },
        { provide: ActivatedRoute, useValue: { snapshot: { paramMap: { get: () => '123' } } } }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(EditPatientComponent);
    component = fixture.componentInstance;
    patientService = TestBed.inject(PatientService) as jasmine.SpyObj<PatientService>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    route = TestBed.inject(ActivatedRoute);

    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load patient data on init', () => {
    const patientData = {
      id: '123',
      name: 'John Doe',
      gender: 'Male',
      dateOfBirth: new Date('1990-01-01'),
      email: 'john.doe@example.com',
      phoneNumber: '1234567890',
      medicalRecordNumber: '123',
      address: {
        street: '123 Main St',
        postalCode: '12345',
        city: 'Anytown',
        country: 'USA'
      },
      emergencyContact: {
        name: 'Jane Doe',
        email: 'jane.doe@example.com',
        phoneNumber: '0987654321'
      },
      medicalRecord: {
        id: 'test',
        patientId: 'test',
        allergies: [],
        medicalConditions: []
      }
    };
    patientService.getPatientByMedicalRecordNumber.and.returnValue(of(patientData));

    component.ngOnInit();

    expect(component.submitForm.name).toBe(patientData.name);
    expect(component.submitForm.email).toBe(patientData.email);
    expect(component.submitForm.phoneNumber).toBe(patientData.phoneNumber);
    expect(component.submitForm.street).toBe(patientData.address.street);
    expect(component.submitForm.postalCode).toBe(patientData.address.postalCode);
    expect(component.submitForm.city).toBe(patientData.address.city);
    expect(component.submitForm.country).toBe(patientData.address.country);
  });

  it('should handle error when loading patient data', () => {
    const error = { error: { message: 'Error loading patient' } };
    patientService.getPatientByMedicalRecordNumber.and.returnValue(throwError(error));

    spyOn(console, 'error');

    component.ngOnInit();

    expect(console.error).toHaveBeenCalledWith('Error loading patient', error);
  });

  it('should confirm submission', () => {
    component.confirmSubmission();
    expect(component.showConfirmation).toBeTrue();
  });

  it('should close confirmation modal', () => {
    component.closeConfirmationModal();
    expect(component.showConfirmation).toBeFalse();
  });

  it('should submit form and navigate on success', () => {
    const patientForm = { valid: true };
    const patientData = {
      name: 'John Doe',
      email: 'john.doe@example.com',
      phoneNumber: '1234567890',
      street: '123 Main St',
      postalCode: '12345',
      city: 'Anytown',
      country: 'USA'
    };
    component.submitForm = patientData;
    component.medicalRecordNumber = '123';
    patientService.editPatient.and.returnValue(of({}));

    spyOn(console, 'error');
    component.onSubmit(patientForm);

    expect(patientService.editPatient).toHaveBeenCalledWith(
      '123',
      patientData.name,
      patientData.email,
      patientData.phoneNumber,
      patientData.street,
      patientData.postalCode,
      patientData.city,
      patientData.country
    );
    expect(messageService.setMessage).toHaveBeenCalledWith('Patient nº 123 successfully edited!');
    expect(router.navigate).toHaveBeenCalledWith(['/patients']);
  });


  it('should handle error on form submission', () => {
    const patientForm = { valid: true };
    const error = { error: { message: 'Failed to edit patient' } };
    component.medicalRecordNumber = '123';
    patientService.editPatient.and.returnValue(throwError(error));

    spyOn(console, 'error');
    component.onSubmit(patientForm);

    expect(component.errorMessage).toBe(error.error.message);
    expect(console.error).toHaveBeenCalledWith('Failed to edit patient', error);
  });

  it('should set error message if medical record number is missing', () => {
    const patientForm = { valid: true };
    component.medicalRecordNumber = null;

    component.onSubmit(patientForm);

    expect(component.errorMessage).toBe('Medical record number is missing.');
  });

  it('should navigate to patients on cancel', () => {
    component.onCancel();
    expect(router.navigate).toHaveBeenCalledWith(['/patients']);
  });
});