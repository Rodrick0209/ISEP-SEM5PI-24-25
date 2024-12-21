import { ComponentFixture, TestBed } from '@angular/core/testing';
import { CreateAppointmentComponent } from './create-appointment.component';
import { AppointmentService } from '../../services/appointment.service';
import { MessageService } from '../../services/message.service';
import { SpecializationService } from '../../services/specialization.service';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { RoomStatus } from '../../models/operationRoom';

describe('CreateAppointmentComponent', () => {
  let component: CreateAppointmentComponent;
  let fixture: ComponentFixture<CreateAppointmentComponent>;
  let appointmentService: jasmine.SpyObj<AppointmentService>;
  let messageService: jasmine.SpyObj<MessageService>;
  let specializationService: jasmine.SpyObj<SpecializationService>;
  let router: jasmine.SpyObj<Router>;

  beforeEach(async () => {
    const appointmentServiceSpy = jasmine.createSpyObj('AppointmentService', ['getOperationRooms', 'getOperationRequestsAvailable', 'getTeamForAppointmentCreate', 'createAppointmentWithMedicalTeam']);
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['setMessage']);
    const specializationServiceSpy = jasmine.createSpyObj('SpecializationService', ['getSpecializationById']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      providers: [
        { provide: AppointmentService, useValue: appointmentServiceSpy },
        { provide: MessageService, useValue: messageServiceSpy },
        { provide: SpecializationService, useValue: specializationServiceSpy },
        { provide: Router, useValue: routerSpy }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(CreateAppointmentComponent);
    component = fixture.componentInstance;
    appointmentService = TestBed.inject(AppointmentService) as jasmine.SpyObj<AppointmentService>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
    specializationService = TestBed.inject(SpecializationService) as jasmine.SpyObj<SpecializationService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    appointmentService.createAppointmentWithMedicalTeam.and.returnValue(of({}));
    specializationService.getSpecializationById.and.returnValue(of({ id: '1', name: 'Anesthesiologist' }));
    appointmentService.getOperationRequestsAvailable.and.returnValue(of([]));
    appointmentService.getOperationRooms.and.returnValue(of([]));
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load operation rooms on init', () => {
    const mockRooms = [{ id: 1, name: 'Room 1', roomNumber: '101', capacity: '2', roomStatus: RoomStatus.Available }];

    appointmentService.getOperationRooms.and.returnValue(of(mockRooms));

    component.ngOnInit();

    expect(appointmentService.getOperationRooms).toHaveBeenCalled();
    expect(component.operationRooms).toEqual(mockRooms);
  });

  it('should handle error when loading operation rooms', () => {
    const error = new Error('Failed to load operation rooms');
    appointmentService.getOperationRooms.and.returnValue(throwError(error));

    component.ngOnInit();

    expect(appointmentService.getOperationRooms).toHaveBeenCalled();
    expect(component.operationRooms).toEqual([]);
  });

  it('should load operation requests on init', () => {
    const mockRequests = [{ id: '1',  patientId: '1', operationTypeId: '1', operationDate: '2023-10-10', operationTime: '10:30', operationRoomId: '1', doctorThatWillPerformId: 'Dr. Smith', deadLineDate: new Date(), priority: 'Pending' }];
    appointmentService.getOperationRequestsAvailable.and.returnValue(of(mockRequests));

    component.ngOnInit();

    expect(appointmentService.getOperationRequestsAvailable).toHaveBeenCalled();
    expect(component.operationRequests).toEqual(mockRequests);
  });

  it('should handle error when loading operation requests', () => {
    const error = new Error('Failed to load operation requests');
    appointmentService.getOperationRequestsAvailable.and.returnValue(throwError(error));

    component.ngOnInit();

    expect(appointmentService.getOperationRequestsAvailable).toHaveBeenCalled();
    expect(component.operationRequests).toEqual([]);
  });

  it('should load medical team to show', () => {
    const mockMedicalTeam = {
      staffAnesthesyPhase: [{ specializationId: '1', nrNeededStaff: 2, staffId: ['1', '2'] }],
      staffSurgeryPhase: [{ specializationId: '2', nrNeededStaff: 3, staffId: ['1', '2', '3'] }]
    };
    component.submitForm = {
      appointmentTimeSlotDate: '2023-10-10',
      appointmentTimeSlotStartMinute: '30',
      operationRoomId: '1',
      operationRequestId: '1'
    };
    appointmentService.getTeamForAppointmentCreate.and.returnValue(of(mockMedicalTeam));

    component.loadMedicalTeamToShow();

    expect(appointmentService.getTeamForAppointmentCreate).toHaveBeenCalled();
    expect(component.medicalTeamToShows).toEqual(mockMedicalTeam);
  });

  it('should handle error when loading medical team to show', () => {
    const error = new Error('Failed to load medical team');
    component.submitForm = {
      appointmentTimeSlotDate: '2023-10-10',
      appointmentTimeSlotStartMinute: '30',
      operationRoomId: '1',
      operationRequestId: '1'
    };
    appointmentService.getTeamForAppointmentCreate.and.returnValue(throwError(error));

    component.loadMedicalTeamToShow();

    expect(appointmentService.getTeamForAppointmentCreate).toHaveBeenCalled();
    expect(component.errorMessage).toBe('Please choose another date or start time, there is no medical team available for the selected date and time.');
  });

  it('should confirm submission', () => {
    component.confirmSubmission();
    expect(component.showConfirmation).toBeTrue();
  });

  it('should close confirmation modal', () => {
    component.closeConfirmationModal();
    expect(component.showConfirmation).toBeFalse();
  });

  it('should navigate to appointments on cancel', () => {
    component.onCancel();
    expect(router.navigate).toHaveBeenCalledWith(['/appointments']);
  });

  it('should validate medical team completion', () => {
    component.medicalTeamToShows = {
      staffAnesthesyPhase: [{ specializationId: '1', nrNeededStaff: 2, staffId: ['1', '2'] }],
      staffSurgeryPhase: [{ specializationId: '2', nrNeededStaff: 3, staffId: ['1', '2', '3'] }]
    };
    component.staffAnestesiaEscolhido = { '1': ['staff1', 'staff2'] };
    component.staffCirurgiaEscolhido = { '2': ['staff3', 'staff4', 'staff5'] };

    const isValid = component.validateMedicalTeamCompletion();

    expect(isValid).toBeTrue();
  });

  it('should invalidate medical team completion', () => {
    component.medicalTeamToShows = {
      staffAnesthesyPhase: [{ specializationId: '1', nrNeededStaff: 2, staffId: ['1', '2'] }],
      staffSurgeryPhase: [{ specializationId: '2', nrNeededStaff: 3, staffId: ['1', '2', '3'] }]
    };
    component.staffAnestesiaEscolhido = { '1': ['staff1'] };
    component.staffCirurgiaEscolhido = { '2': ['staff3', 'staff4'] };

    const isValid = component.validateMedicalTeamCompletion();

    expect(isValid).toBeFalse();
  });
});