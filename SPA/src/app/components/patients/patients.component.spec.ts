
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { PatientsComponent } from './patients.component';
import { ListPatientsComponent } from '../list-patients/list-patients.component';
import { CreatePatientComponent } from '../create-patient/create-patient.component';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { PatientService } from '../../services/patient.service';
import { of } from 'rxjs/internal/observable/of';



describe('PatientsComponent', () => {
  let patientService: jasmine.SpyObj<PatientService>;
  let component: PatientsComponent;
  let fixture: ComponentFixture<PatientsComponent>;

  beforeEach(async () => {
    const patientServiceSpy = jasmine.createSpyObj('PatientService', ['getPatients', 'filterPatients']);
    patientServiceSpy.getPatients.and.returnValue(of([]));
    await TestBed.configureTestingModule({
      imports: [PatientsComponent, ListPatientsComponent, CreatePatientComponent],
      providers: [
        { provide: PatientService, useValue: patientServiceSpy }
      ],
      schemas: [NO_ERRORS_SCHEMA]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(PatientsComponent);
    component = fixture.componentInstance;
    patientService = TestBed.inject(PatientService) as jasmine.SpyObj<PatientService>;

    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should add a new patient to the list', () => {
    const newPatient = 'John Doe';
    component.onPatientAdded(newPatient);
    expect(component.patients).toContain(newPatient);
  });

  it('should have an empty initial patients list', () => {
    expect(component.patients.length).toBe(0);
  });
});
