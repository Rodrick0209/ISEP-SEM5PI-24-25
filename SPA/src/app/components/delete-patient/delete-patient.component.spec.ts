import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute, Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { DeletePatientComponent } from './delete-patient.component';
import { PatientService } from '../../services/patient.service';
import { MessageService } from '../../services/message.service';
import { CommonModule } from '@angular/common';
import { MarkXComponent } from '../template/mark-x/mark-x.component';


describe('DeletePatientComponent', () => {
  let component: DeletePatientComponent;
  let fixture: ComponentFixture<DeletePatientComponent>;
  let patientService: jasmine.SpyObj<PatientService>;
  let messageService: jasmine.SpyObj<MessageService>;
  let router: jasmine.SpyObj<Router>;
  let activatedRoute: ActivatedRoute;

  beforeEach(async () => {
    const patientServiceSpy = jasmine.createSpyObj('PatientService', ['deletePatient']);
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['setMessage']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [DeletePatientComponent, CommonModule],
      providers: [
        { provide: PatientService, useValue: patientServiceSpy },
        { provide: MessageService, useValue: messageServiceSpy },
        { provide: Router, useValue: routerSpy },
        {
          provide: ActivatedRoute,
          useValue: {
            snapshot: {
              paramMap: {
                get: (key: string) => '12345'
              }
            }
          }
        }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(DeletePatientComponent);
    component = fixture.componentInstance;
    patientService = TestBed.inject(PatientService) as jasmine.SpyObj<PatientService>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    activatedRoute = TestBed.inject(ActivatedRoute);
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize medicalRecordNumber on init', () => {
    component.ngOnInit();
    expect(component.medicalRecordNumber).toBe('12345');
  });

  it('should call deletePatient and navigate on successful deletion', () => {
    component.isConfirmed = true;
    component.medicalRecordNumber = '12345';
    patientService.deletePatient.and.returnValue(of({}));

    component.onDelete();

    expect(patientService.deletePatient).toHaveBeenCalledWith('12345');
    expect(messageService.setMessage).toHaveBeenCalledWith('Patient nº 12345 successfully deleted!');
    expect(router.navigate).toHaveBeenCalledWith(['/patients']);
  });

  it('should set errorMessage on failed deletion', () => {
    component.isConfirmed = true;
    component.medicalRecordNumber = '12345';
    patientService.deletePatient.and.returnValue(throwError(() => new Error('Failed to delete patient')));

    component.onDelete();

    expect(patientService.deletePatient).toHaveBeenCalledWith('12345');
    expect(component.errorMessage).toBe('Failed to delete patient');
  });

  it('should navigate to patients list on cancel', () => {
    component.onCancel();
    expect(router.navigate).toHaveBeenCalledWith(['/patients']);
  });
});

