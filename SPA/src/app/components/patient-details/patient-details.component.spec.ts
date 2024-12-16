import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute } from '@angular/router';
import { of, throwError } from 'rxjs';
import { PatientDetailsComponent } from './patient-details.component';
import { PatientService } from '../../services/patient.service';
import { CommonModule } from '@angular/common';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { MedicalConditionsComponent } from '../medical-conditions/medical-conditions.component';


describe('PatientDetailsComponent', () => {
  let component: PatientDetailsComponent;
  let fixture: ComponentFixture<PatientDetailsComponent>;
  let patientService: jasmine.SpyObj<PatientService>;
  let route: ActivatedRoute;

  beforeEach(() => {
    const patientServiceSpy = jasmine.createSpyObj('PatientService', ['getPatientByMedicalRecordNumber']);
    route = { snapshot: { paramMap: { get: () => '2024100000001' } } } as any;

    TestBed.configureTestingModule({
      imports: [CommonModule, HttpClientTestingModule],
      providers: [
        { provide: PatientService, useValue: patientServiceSpy },
        { provide: ActivatedRoute, useValue: route }
      ]
    }).compileComponents();
    fixture = TestBed.createComponent(PatientDetailsComponent);
    component = fixture.componentInstance;
    patientService = TestBed.inject(PatientService) as jasmine.SpyObj<PatientService>;
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should fetch patient details on init', () => {
    const patientData = {
      id: 'test',
      name: 'John Doe',
      medicalRecordNumber: '202410000001',
      dateOfBirth: new Date('1990-01-01'),
      email: 'john.doe@example.com',
      phoneNumber: '123-456-7890',
      gender: 'Male',
      address: {
        street: '123 Main St',
        postalCode: '12345',
        city: 'Springfield',
        country: 'USA'
      },
      emergencyContact: {
        name: 'Jane Doe',
        email: 'jane.doe@example.com',
        phoneNumber: '987-654-3210'
      },
      medicalRecord: {
        id: 'test',
        patientId: 'test',
        allergies: [],
        medicalConditions: [],
      }
    };
    patientService.getPatientByMedicalRecordNumber.and.returnValue(of(patientData));

    component.ngOnInit();

    expect(patientService.getPatientByMedicalRecordNumber).toHaveBeenCalledWith('2024100000001');
    expect(component.patient).toEqual(patientData);
    expect(component.patient?.medicalRecordNumber).toBe('202410000001');
    expect(component.patient?.name).toBe('John Doe');
    expect(component.patient?.dateOfBirth).toEqual(new Date('1990-01-01'));
    expect(component.patient?.email).toBe('john.doe@example.com');
    expect(component.patient?.phoneNumber).toBe('123-456-7890');
    expect(component.patient?.gender).toBe('Male'),
    expect(component.patient?.address?.street).toBe('123 Main St');
    expect(component.patient?.address?.postalCode).toBe('12345');
    expect(component.patient?.address?.city).toBe('Springfield');
    expect(component.patient?.address?.country).toBe('USA');
    expect(component.patient?.emergencyContact?.name).toBe('Jane Doe');
    expect(component.patient?.emergencyContact?.email).toBe('jane.doe@example.com');
    expect(component.patient?.emergencyContact?.phoneNumber).toBe('987-654-3210');
    expect(component.errorMessage).toBe('');
  });  

  it('should handle error when fetching patient details', () => {
    patientService.getPatientByMedicalRecordNumber.and.returnValue(throwError('Error'));

    component.ngOnInit();

    expect(patientService.getPatientByMedicalRecordNumber).toHaveBeenCalledWith('2024100000001');
    expect(component.patient).toBeUndefined();
    expect(component.errorMessage).toBe('Failed to fetch patient details');
  });

  it('should set error message if medicalRecordNumber is not provided', () => {
    route.snapshot.paramMap.get = () => null;

    component.ngOnInit();

    expect(component.errorMessage).toBe('Invalid patient');
  });
});
