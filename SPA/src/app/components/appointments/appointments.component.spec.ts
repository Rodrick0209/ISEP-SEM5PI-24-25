import { ComponentFixture, TestBed } from '@angular/core/testing';
import { AppointmentsComponent } from './appointments.component';
import { ListAppointmentsComponent } from '../list-appointments/list-appointments.component';
import { CreateAppointmentComponent } from '../create-appointment/create-appointment.component';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { AppointmentService } from '../../services/appointment.service';
import { of } from 'rxjs';

describe('AppointmentsComponent', () => {
  let appointmentService: jasmine.SpyObj<AppointmentService>;
  let component: AppointmentsComponent;
  let fixture: ComponentFixture<AppointmentsComponent>;

  beforeEach(async () => {
    const appointmentServiceSpy = jasmine.createSpyObj('AppointmentService', ['getAppointments']);
    appointmentServiceSpy.getAppointments.and.returnValue(of([]));

    await TestBed.configureTestingModule({
      imports: [AppointmentsComponent, ListAppointmentsComponent, CreateAppointmentComponent],
      providers: [
        { provide: AppointmentService, useValue: appointmentServiceSpy }
      ],
      schemas: [NO_ERRORS_SCHEMA]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AppointmentsComponent);
    component = fixture.componentInstance;
    appointmentService = TestBed.inject(AppointmentService) as jasmine.SpyObj<AppointmentService>;

    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });


  it('should have an empty initial appointment list', () => {
    expect(component.appointments.length).toBe(0);
  });
});