import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ListMedicalRecordEntriesComponent } from './list-medical-record-entries.component';
import { PatientService } from '../../services/patient.service';
import { MessageService } from '../../services/message.service';
import { ActivatedRoute, Router } from '@angular/router';
import { of, throwError } from 'rxjs';

describe('ListMedicalRecordEntriesComponent', () => {
  let component: ListMedicalRecordEntriesComponent;
  let fixture: ComponentFixture<ListMedicalRecordEntriesComponent>;
  let patientServiceMock: any;
  let messageServiceMock: any;
  let routerMock: any;
  let activatedRouteMock: any;

  beforeEach(async () => {
    patientServiceMock = jasmine.createSpyObj('PatientService', ['getMedicalRecordByPatientId', 'filterMedicalRecordEntries']);
    messageServiceMock = jasmine.createSpyObj('MessageService', ['getMessage']);
    routerMock = jasmine.createSpyObj('Router', ['navigate']);
    activatedRouteMock = { snapshot: { paramMap: { get: jasmine.createSpy('get').and.returnValue('123') } } };

    await TestBed.configureTestingModule({
      providers: [
        { provide: PatientService, useValue: patientServiceMock },
        { provide: MessageService, useValue: messageServiceMock },
        { provide: Router, useValue: routerMock },
        { provide: ActivatedRoute, useValue: activatedRouteMock }
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    patientServiceMock.getMedicalRecordByPatientId.and.returnValue(of({ id: '1', patientId: '1', allergies: [], medicalConditions: [] }));
    fixture = TestBed.createComponent(ListMedicalRecordEntriesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize successMessage and medicalRecordNumber on init', () => {
    messageServiceMock.getMessage.and.returnValue('Success');
    component.ngOnInit();
    expect(component.successMessage).toBe('Success');
    expect(component.medicalRecordNumber).toBe('123');
  });

  it('should call getMedicalRecord on init', () => {
    spyOn(component, 'getMedicalRecord');
    component.ngOnInit();
    expect(component.getMedicalRecord).toHaveBeenCalled();
  });

  it('should get medical record on getMedicalRecord call', () => {
    const mockRecord = { id: '1', patientId: '1', allergies: [], medicalConditions: [] };
    patientServiceMock.getMedicalRecordByPatientId.and.returnValue(of(mockRecord));
    component.getMedicalRecord();
    expect(component.medicalRecord).toEqual(mockRecord);
  });

  it('should handle error on getMedicalRecord call', () => {
    patientServiceMock.getMedicalRecordByPatientId.and.returnValue(throwError('Error'));
    component.getMedicalRecord();
    expect(component.errorMessage).toBe('Failed to get medical record');
  });

  it('should filter medical record entries on onFilterChanged call', () => {
    const mockRecord = { id: '1', patientId: '1', allergies: [], medicalConditions: [] };
    patientServiceMock.filterMedicalRecordEntries.and.returnValue(of(mockRecord));
    component.onFilterChanged({ designation: 'test' });
    expect(component.medicalRecord).toEqual(mockRecord);
  });

  it('should navigate to edit medical record on updateMedicalRecord call', () => {
    component.updateMedicalRecord();
    expect(routerMock.navigate).toHaveBeenCalledWith(['/edit-medical-record', '123']);
  });
});