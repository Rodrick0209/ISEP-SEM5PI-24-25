import { ComponentFixture, TestBed } from '@angular/core/testing';
import { EditAppointmentComponent } from './edit-appointment.component';
import { AppointmentService } from '../../services/appointment.service';
import { MessageService } from '../../services/message.service';
import { Router } from '@angular/router';
import { ActivatedRoute } from '@angular/router';
import { of, throwError } from 'rxjs';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

describe('EditAppointmentComponent', () => {
  let component: EditAppointmentComponent;
  let fixture: ComponentFixture<EditAppointmentComponent>;
  let mockAppointmentService: jasmine.SpyObj<AppointmentService>;
  let mockMessageService: jasmine.SpyObj<MessageService>;
  let mockRouter: jasmine.SpyObj<Router>;

  beforeEach(async () => {
    mockAppointmentService = jasmine.createSpyObj('AppointmentService', ['getAppointmentByIdEdit', 'editAppointment', 'getStaff']);
    mockMessageService = jasmine.createSpyObj('MessageService', ['setMessage']);
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      declarations: [ EditAppointmentComponent ],
      imports: [ FormsModule, CommonModule ],
      providers: [
        { provide: AppointmentService, useValue: mockAppointmentService },
        { provide: MessageService, useValue: mockMessageService },
        { provide: Router, useValue: mockRouter },
        { provide: ActivatedRoute, useValue: { snapshot: { paramMap: { get: () => '123' } } } }
      ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(EditAppointmentComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load appointment details when initialized', () => {
    const mockAppointment = {
      appointmentTimeSlot: {
        date: '2024-12-08',
        startTime: '09:00',
        endTime: '10:00'
      },
      operationRoomId: 'room1',
      operationRequestId: 'req123',
      appointmentStatus: 'Scheduled',
      operationRequestTeamForAnesthesy: ['Anesthesia Team'],
      operationRequestTeamForSurgery: ['Surgery Team']
    };
    mockAppointmentService.getAppointmentByIdEdit.and.returnValue(of(mockAppointment));

    component.ngOnInit();

    expect(component.submitForm.appointmentTimeSlotDtoDate).toBe(mockAppointment.appointmentTimeSlot.date);
    expect(component.submitForm.operationRoomId).toBe(mockAppointment.operationRoomId);
    expect(component.submitForm.operationRequestId).toBe(mockAppointment.operationRequestId);
  });

  it('should handle error when appointment details fail to load', () => {
    mockAppointmentService.getAppointmentByIdEdit.and.returnValue(throwError(() => new Error('Failed to fetch appointment details')));

    component.ngOnInit();

    expect(component.errorMessage).toBe('Failed to fetch appointment details.');
  });

  it('should load staff list when initialized', () => {
    const mockStaffList = [{ id: 'staff1', name: 'Dr. Smith' }, { id: 'staff2', name: 'Dr. Johnson' }];
    mockAppointmentService.getStaff.and.returnValue(of(mockStaffList));

    component.ngOnInit();

    expect(component.availableStaff).toEqual(mockStaffList);
  });

  it('should handle error when staff list fails to load', () => {
    mockAppointmentService.getStaff.and.returnValue(throwError(() => new Error('Failed to fetch staff list')));

    component.ngOnInit();

    expect(component.errorMessage).toBe('Failed to fetch staff list');
  });

  it('should submit form and navigate to appointments page', () => {
    const mockAppointmentData = {
      operationRequestId: 'req123',
      operationRoomId: 'room1',
      appointmentTimeSlotDtoDate: '2024-12-08',
      appointmentTimeSlotDtoTimeSlotStartMinute: '540',
      appointmentTimeSlotDtoTimeSlotEndMinute: '600',
      appointmentStatus: 'Scheduled',
      operationRequestTeamForAnesthesy: ['Anesthesia Team'],
      operationRequestTeamForSurgery: ['Surgery Team']
    };

    mockAppointmentService.editAppointment.and.returnValue(of({ success: true }));

    component.submitForm = mockAppointmentData;

    component.onSubmit({ valid: true });

    expect(mockAppointmentService.editAppointment).toHaveBeenCalledWith(
      '123',
      mockAppointmentData.operationRequestId,
      mockAppointmentData.operationRoomId,
      mockAppointmentData.appointmentTimeSlotDtoDate,
      mockAppointmentData.appointmentTimeSlotDtoTimeSlotStartMinute,
      mockAppointmentData.appointmentTimeSlotDtoTimeSlotEndMinute,
      mockAppointmentData.appointmentStatus,
      mockAppointmentData.operationRequestTeamForAnesthesy,
      mockAppointmentData.operationRequestTeamForSurgery
    );
    expect(mockMessageService.setMessage).toHaveBeenCalledWith('Appointment 123 successfully edited!');
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/appointments']);
  });

  it('should handle error when submitting the form', () => {
    const mockAppointmentData = {
      operationRequestId: 'req123',
      operationRoomId: 'room1',
      appointmentTimeSlotDtoDate: '2024-12-08',
      appointmentTimeSlotDtoTimeSlotStartMinute: '540',
      appointmentTimeSlotDtoTimeSlotEndMinute: '600',
      appointmentStatus: 'Scheduled',
      operationRequestTeamForAnesthesy: ['Anesthesia Team'],
      operationRequestTeamForSurgery: ['Surgery Team']
    };

    mockAppointmentService.editAppointment.and.returnValue(throwError(() => new Error('Failed to edit appointment')));

    component.submitForm = mockAppointmentData;

    component.onSubmit({ valid: true });

    expect(component.errorMessage).toBe('Failed to edit appointment');
  });

  it('should cancel and navigate back to appointments', () => {
    component.onCancel();

    expect(mockRouter.navigate).toHaveBeenCalledWith(['/appointments']);
  });
});
