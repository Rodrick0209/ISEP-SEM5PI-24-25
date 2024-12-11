import { ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { FormsModule } from '@angular/forms';
import { AppointmentComponent } from './appointment.component';
import { AppointmentService } from '../../services/appointment.service';
import { MessageService } from '../../services/message.service';

describe('AppointmentComponent', () => {
  let component: AppointmentComponent;
  let fixture: ComponentFixture<AppointmentComponent>;
  let mockRouter: jasmine.SpyObj<Router>;
  let mockAppointmentService: jasmine.SpyObj<AppointmentService>;
  let mockMessageService: jasmine.SpyObj<MessageService>;

  beforeEach(async () => {
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);
    mockAppointmentService = jasmine.createSpyObj('AppointmentService', ['createAppointment']);
    mockMessageService = jasmine.createSpyObj('MessageService', ['setMessage']);

    await TestBed.configureTestingModule({
      imports: [FormsModule],
      declarations: [AppointmentComponent],
      providers: [
        { provide: Router, useValue: mockRouter },
        { provide: AppointmentService, useValue: mockAppointmentService },
        { provide: MessageService, useValue: mockMessageService },
      ],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AppointmentComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should navigate to appointments list on cancel', () => {
    component.onCancel();
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/appointments']);
  });

  it('should show confirmation modal', () => {
    component.confirmSubmission();
    expect(component.showConfirmation).toBeTrue();
  });

  it('should close confirmation modal', () => {
    component.closeConfirmationModal();
    expect(component.showConfirmation).toBeFalse();
  });

  it('should submit form successfully', () => {
    const appointmentForm = { valid: true };
    const appointmentData = {
      appointmentTimeSlotDate: '2024-12-01',
      appointmentTimeSlotStartMinute: 600, // Example value in minutes (e.g., 10:00 AM)
      appointmentTimeSlotEndMinute: 660, // Example value in minutes (e.g., 11:00 AM)
      operationRoomId: 101,
      operationRequestId: 2024,
    };
    component.submitForm = appointmentData;
    mockAppointmentService.createAppointment.and.returnValue(of({}));

    component.onSubmit(appointmentForm);

    expect(mockAppointmentService.createAppointment).toHaveBeenCalledWith(
      appointmentData.appointmentTimeSlotDate,
      appointmentData.appointmentTimeSlotStartMinute,
      appointmentData.appointmentTimeSlotEndMinute,
      appointmentData.operationRoomId,
      appointmentData.operationRequestId
    );
    expect(mockMessageService.setMessage).toHaveBeenCalledWith(
      'Appointment successfully scheduled!'
    );
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/appointments']);
  });

  it('should handle form submission error', () => {
    const appointmentForm = { valid: true };
    const appointmentData = {
      appointmentTimeSlotDate: '2024-12-01',
      appointmentTimeSlotStartMinute: 600,
      appointmentTimeSlotEndMinute: 660,
      operationRoomId: 101,
      operationRequestId: 2024,
    };
    component.submitForm = appointmentData;
    mockAppointmentService.createAppointment.and.returnValue(
      throwError({ error: { message: 'Error' } })
    );

    component.onSubmit(appointmentForm);

    expect(mockAppointmentService.createAppointment).toHaveBeenCalledWith(
      appointmentData.appointmentTimeSlotDate,
      appointmentData.appointmentTimeSlotStartMinute,
      appointmentData.appointmentTimeSlotEndMinute,
      appointmentData.operationRoomId,
      appointmentData.operationRequestId
    );
    expect(component.errorMessage).toBe('Failed to schedule appointment: Error');
  });

  it('should log invalid form', () => {
    spyOn(console, 'log');
    const appointmentForm = { valid: false };

    component.onSubmit(appointmentForm);

    expect(console.log).toHaveBeenCalledWith('Form is invalid');
  });
});
