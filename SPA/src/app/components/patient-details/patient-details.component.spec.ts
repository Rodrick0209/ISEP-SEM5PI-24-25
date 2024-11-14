import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute } from '@angular/router';
import { of, throwError } from 'rxjs';
import { PatientDetailsComponent } from './patient-details.component';
import { PatientService } from '../../services/patient.service';
import { CommonModule } from '@angular/common';


describe('PatientDetailsComponent', () => {
  let component: PatientDetailsComponent;
  let fixture: ComponentFixture<PatientDetailsComponent>;
  let patientService: jasmine.SpyObj<PatientService>;
  let route: ActivatedRoute;

  beforeEach(() => {
    const patientServiceSpy = jasmine.createSpyObj('PatientService', ['getPatientByMedicalRecordNumber']);
    route = { snapshot: { paramMap: { get: () => '2024100000001' } } } as any;

    TestBed.configureTestingModule({
      imports: [CommonModule, PatientDetailsComponent],
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


  it('should fetch and display patient details correctly', () => {
    const patientData = {
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
      medicalHistory: {
        medicalConditions: 'Asthma'
      },
      emergencyContact: {
        name: 'Jane Doe',
        email: 'jane.doe@example.com',
        phoneNumber: '987-654-3210'
      }
    };
    
    patientService.getPatientByMedicalRecordNumber.and.returnValue(of(patientData));

    component.ngOnInit();
    fixture.detectChanges();

    const compiled = fixture.nativeElement;
    expect(compiled.querySelector('.patient-header h2')?.textContent).toContain('Patient Details');
    expect(compiled.querySelector('.patient-info .patient-field:nth-child(1)')?.textContent).toContain('202410000001');
    expect(compiled.querySelector('.patient-info .patient-field:nth-child(2)')?.textContent).toContain('John Doe');
    expect(compiled.querySelector('.patient-info .patient-field:nth-child(3)')?.textContent).toContain('1990-01-01');
    expect(compiled.querySelector('.patient-info .patient-field:nth-child(4)')?.textContent).toContain('Male');
    expect(compiled.querySelector('.patient-info .patient-field:nth-child(5)')?.textContent).toContain('joh***@***.com');
    expect(compiled.querySelector('.patient-info .patient-field:nth-child(6)')?.textContent).toContain('123***90');
    
    expect(compiled.querySelector('.address .patient-field:nth-child(1)')?.textContent).toContain('123 Main St');
    expect(compiled.querySelector('.address .patient-field:nth-child(2)')?.textContent).toContain('12345');
    expect(compiled.querySelector('.address .patient-field:nth-child(3)')?.textContent).toContain('Springfield');
    expect(compiled.querySelector('.address .patient-field:nth-child(4)')?.textContent).toContain('USA');
    
    expect(compiled.querySelector('.emergency-contact .patient-field:nth-child(1)')?.textContent).toContain('Jane Doe');
    expect(compiled.querySelector('.emergency-contact .patient-field:nth-child(2)')?.textContent).toContain('jan***@***.com');
    expect(compiled.querySelector('.emergency-contact .patient-field:nth-child(3)')?.textContent).toContain('987***10');
    
    expect(compiled.querySelector('.medical-history .patient-field')?.textContent).toContain('Asthma');
  });

  it('should fetch patient details on init', () => {
    const patientData = { medicalRecordNumber: '2024100000001', name: 'John Doe' } as any;
    patientService.getPatientByMedicalRecordNumber.and.returnValue(of(patientData));

    component.ngOnInit();

    expect(patientService.getPatientByMedicalRecordNumber).toHaveBeenCalledWith('12345');
    expect(component.patient).toEqual(patientData);
    expect(component.errorMessage).toBe('');
  });  

  it('should handle error when fetching patient details', () => {
    patientService.getPatientByMedicalRecordNumber.and.returnValue(throwError('Error'));

    component.ngOnInit();

    expect(patientService.getPatientByMedicalRecordNumber).toHaveBeenCalledWith('12345');
    expect(component.patient).toBeUndefined();
    expect(component.errorMessage).toBe('Failed to fetch patient details');
  });

  it('should set error message if medicalRecordNumber is not provided', () => {
    route.snapshot.paramMap.get = () => null;

    component.ngOnInit();

    expect(component.errorMessage).toBe('Invalid patient');
  });
});
