import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ListAppointmentsComponent } from './list-appointments.component';
import { AppointmentService } from '../../services/appointment.service';
import { Router } from '@angular/router';
import { of } from 'rxjs';
import { AppointmentStatus, AppointmentsView } from '../../models/appointment';
import { By } from '@angular/platform-browser';

describe('ListAppointmentsComponent', () => {
  let component: ListAppointmentsComponent;
  let fixture: ComponentFixture<ListAppointmentsComponent>;
  let appointmentServiceMock: jasmine.SpyObj<AppointmentService>;
  let routerMock: jasmine.SpyObj<Router>;

  beforeEach(async () => {
    // Criação de mocks dos serviços
    appointmentServiceMock = jasmine.createSpyObj('AppointmentService', ['getAppointments']);
    routerMock = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      declarations: [ ListAppointmentsComponent ],
      providers: [
        { provide: AppointmentService, useValue: appointmentServiceMock },
        { provide: Router, useValue: routerMock }
      ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ListAppointmentsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('should load appointments on init', () => {
    const mockAppointments: AppointmentsView[] = [
      {
        id: '1',
        operationRequestPriority: 'High',
        operationRequestPatientId: '123',
        appointmentTimeSlot: { date: new Date().toISOString().split('T')[0], startTime: new Date().toISOString(), endTime: new Date().toISOString() },
        appointmentStatus: AppointmentStatus.Scheduled,
        operationRoomNumber: '101'
      }
    ];

    appointmentServiceMock.getAppointments.and.returnValue(of(mockAppointments));

    component.ngOnInit(); // Chama o ngOnInit para simular a inicialização do componente
    fixture.detectChanges();

    expect(component.paginatedAppointments.length).toBe(1);
    expect(component.paginatedAppointments[0].id).toBe('1');
  });

  it('should display no appointments message when there are no appointments', () => {
    appointmentServiceMock.getAppointments.and.returnValue(of([]));

    component.ngOnInit();
    fixture.detectChanges();

    const noAppointmentsMessage = fixture.debugElement.query(By.css('.no-appointments-message'));
    expect(noAppointmentsMessage).toBeTruthy();
    expect(noAppointmentsMessage.nativeElement.textContent).toContain('No appointments to show.');
  });

  it('should navigate to create appointment page when create appointment button is clicked', () => {
    const createButton = fixture.debugElement.query(By.css('.create-appointment-button'));
    createButton.triggerEventHandler('click', null);

    expect(routerMock.navigate).toHaveBeenCalledWith(['/appointments/create']);
  });

  it('should navigate to edit appointment page when edit button is clicked', () => {
    const mockAppointment: AppointmentsView = {
      id: '1',
      operationRequestPriority: 'High',
      operationRequestPatientId: '123',
      appointmentTimeSlot: { date: new Date().toISOString().split('T')[0], startTime: new Date().toISOString(), endTime: new Date().toISOString() },
      appointmentStatus: AppointmentStatus.Scheduled,
      operationRoomNumber: '101'
    };

    component.editAppointment(mockAppointment);

    expect(routerMock.navigate).toHaveBeenCalledWith(['/appointments/edit', mockAppointment.id]);
  });

  it('should update pagination when changing page', () => {
    const mockAppointments: AppointmentsView[] = [
      {
        id: '1',
        operationRequestPriority: 'High',
        operationRequestPatientId: '123',
        appointmentTimeSlot: { date: new Date().toISOString().split('T')[0], startTime: new Date().toISOString(), endTime: new Date().toISOString() },
        appointmentStatus: AppointmentStatus.Scheduled,
        operationRoomNumber: '101'
      },
      {
        id: '2',
        operationRequestPriority: 'Low',
        operationRequestPatientId: '124',
        appointmentTimeSlot: { date: new Date().toISOString().split('T')[0], startTime: new Date().toISOString(), endTime: new Date().toISOString() },
        appointmentStatus: AppointmentStatus.Completed,
        operationRoomNumber: '102'
      }
    ];

    appointmentServiceMock.getAppointments.and.returnValue(of(mockAppointments));

    component.ngOnInit();
    fixture.detectChanges();

    expect(component.paginatedAppointments.length).toBe(2);
    component.nextPage();
    fixture.detectChanges();

    expect(component.currentPage).toBe(2);
  });
});
