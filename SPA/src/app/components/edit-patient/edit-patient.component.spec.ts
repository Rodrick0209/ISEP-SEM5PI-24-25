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

  beforeEach(async () => {
    const patientServiceSpy = jasmine.createSpyObj('PatientService', ['editPatient']);
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['setMessage']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [EditPatientComponent,FormsModule, CommonModule],
      providers: [
        { provide: PatientService, useValue: patientServiceSpy },
        { provide: MessageService, useValue: messageServiceSpy },
        { provide: Router, useValue: routerSpy },
        {
          provide: ActivatedRoute,
          useValue: {
            snapshot: { paramMap: { get: () => '123' } }
          }
        }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(EditPatientComponent);
    component = fixture.componentInstance;
    patientService = TestBed.inject(PatientService) as jasmine.SpyObj<PatientService>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize medicalRecordNumber on ngOnInit', () => {
    component.ngOnInit();
    expect(component.medicalRecordNumber).toBe('123');
  });

  it('should show confirmation modal on confirmSubmission', () => {
    component.confirmSubmission();
    expect(component.showConfirmation).toBeTrue();
  });

  it('should close confirmation modal on closeConfirmationModal', () => {
    component.closeConfirmationModal();
    expect(component.showConfirmation).toBeFalse();
  });

  it('should navigate to /patients on onCancel', () => {
    component.onCancel();
    expect(router.navigate).toHaveBeenCalledWith(['/patients']);
  });

  it('should call editPatient on valid form submission', () => {
    const patientForm = { valid: true };
    patientService.editPatient.and.returnValue(of({}));
    component.onSubmit(patientForm);
    expect(patientService.editPatient).toHaveBeenCalled();
  });

  it('should set errorMessage on failed editPatient call', () => {
    const patientForm = { valid: true };
    const errorResponse = { error: { message: 'Error' } };
    patientService.editPatient.and.returnValue(throwError(errorResponse));
    component.onSubmit(patientForm);
    expect(component.errorMessage).toBe('Error');
  });

  it('should set errorMessage if medicalRecordNumber is missing', () => {
    component.medicalRecordNumber = null;
    const patientForm = { valid: true };
    component.onSubmit(patientForm);
    expect(component.errorMessage).toBe('Medical record number is missing.');
  });
});