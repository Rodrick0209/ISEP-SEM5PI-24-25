import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ListMedicalRecordEntriesComponent } from './list-medical-record-entries.component';
import { PatientService } from '../../services/patient.service';
import { MessageService } from '../../services/message.service';
import { of, throwError } from 'rxjs';
import { Router } from '@angular/router';
import { PatientsView } from '../../models/patient';

describe('ListMedicalRecordEntriesComponent', () => {
  let component: ListMedicalRecordEntriesComponent;
  let fixture: ComponentFixture<ListMedicalRecordEntriesComponent>;
  let patientService: jasmine.SpyObj<PatientService>;
  let messageService: jasmine.SpyObj<MessageService>;
  let router: jasmine.SpyObj<Router>;

  beforeEach(async () => {
    const patientServiceSpy = jasmine.createSpyObj('PatientService', ['getPatients', 'filterMedicalRecordEntries']);
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['getMessage']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      declarations: [ListMedicalRecordEntriesComponent],
      providers: [
        { provide: PatientService, useValue: patientServiceSpy },
        { provide: MessageService, useValue: messageServiceSpy },
        { provide: Router, useValue: routerSpy }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(ListMedicalRecordEntriesComponent);
    component = fixture.componentInstance;
    patientService = TestBed.inject(PatientService) as jasmine.SpyObj<PatientService>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize with success message and patients', () => {
    const mockPatients: PatientsView[] = [{
        medicalRecordNumber: '123', name: 'John Doe',
        dateOfBirth: new Date(),
        email: ''
    }];
    messageService.getMessage.and.returnValue('Success');
    patientService.getPatients.and.returnValue(of(mockPatients));

    component.ngOnInit();

    expect(component.successMessage).toBe('Success');
    expect(component.patients).toEqual(mockPatients);
    expect(component.filteredPatients).toEqual(mockPatients);
  });

  it('should handle error when fetching patients', () => {
    const errorResponse = { error: { message: 'Error fetching patients' } };
    patientService.getPatients.and.returnValue(throwError(errorResponse));

    component.ngOnInit();

    expect(component.errorMessage).toBe('Failed to get patients: Error fetching patients');
  });

  it('should filter patients based on search query', () => {
    component.patients = [
      {
          medicalRecordNumber: '123', name: 'John Doe',
          dateOfBirth: new Date(),
          email: ''
      },
      {
          medicalRecordNumber: '456', name: 'Jane Doe',
          dateOfBirth: new Date(),
          email: ''
      }
    ];

    component.searchQuery = '123';
    component.filterMedicalRecords();

    expect(component.filteredPatients).toEqual([{ medicalRecordNumber: '123', name: 'John Doe', dateOfBirth: new Date(), email: '' }]);
  });

  it('should select a patient', () => {
    const patient: PatientsView = {
        medicalRecordNumber: '123', name: 'John Doe',
        dateOfBirth: new Date(),
        email: ''
    };

    component.selectPatient(patient);

    expect(component.selectedMedicalRecordNumber).toBe('123');
    expect(component.selectedPatient).toEqual(patient);
    expect(component.searchQuery).toBe('123');
    expect(component.filteredPatients).toEqual([patient]);
  });

  it('should deselect a patient', () => {
    component.patients = [
      {
          medicalRecordNumber: '123', name: 'John Doe',
          dateOfBirth: new Date(),
          email: ''
      },
      {
          medicalRecordNumber: '456', name: 'Jane Doe',
          dateOfBirth: new Date(),
          email: ''
      }
    ];
    component.selectedMedicalRecordNumber = '123';
    component.selectedPatient = { medicalRecordNumber: '123', name: 'John Doe', dateOfBirth: new Date(), email: '' };

    component.onRecordNumberDeselected();

    expect(component.selectedMedicalRecordNumber).toBeNull();
    expect(component.selectedPatient).toBeNull();
    expect(component.searchQuery).toBe('');
    expect(component.filteredPatients).toEqual(component.patients);
  });

  it('should update medical record', () => {
    const patient: PatientsView = {
        medicalRecordNumber: '123', name: 'John Doe',
        dateOfBirth: new Date(),
        email: ''
    };
    component.selectedPatient = patient;

    spyOn(console, 'log');
    component.updateMedicalRecord();

    expect(console.log).toHaveBeenCalledWith('Update medical record for', patient);
  });

  it('should filter medical record entries', () => {
    const mockMedicalRecord: any = {
      id: '1',
      details: 'Record details',
      patientId: '123',
      allergies: [],
      medicalConditions: []
    };
    patientService.filterMedicalRecordEntries.and.returnValue(of(mockMedicalRecord));

    component.selectedMedicalRecordNumber = '123';
    component.onFilterChanged({ name: 'filterName' });

    expect(patientService.filterMedicalRecordEntries).toHaveBeenCalledWith('123', 'filterName');
    expect(component.medicalRecord).toEqual(mockMedicalRecord);
  });

  it('should handle error when filtering medical record entries', () => {
    const errorResponse = { error: { message: 'Error filtering records' } };
    patientService.filterMedicalRecordEntries.and.returnValue(throwError(errorResponse));

    component.selectedMedicalRecordNumber = '123';
    component.onFilterChanged({ name: 'filterName' });

    expect(component.medicalRecord).toBeUndefined();
  });
});