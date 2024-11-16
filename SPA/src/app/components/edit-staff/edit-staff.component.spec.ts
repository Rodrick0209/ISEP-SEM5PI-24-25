import { ComponentFixture, TestBed } from '@angular/core/testing';
import { EditStaffComponent } from './edit-staff.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule } from '@angular/forms';
import { StaffService } from '../../services/staff.service';
import { MessageService } from '../../services/message.service';
import { of } from 'rxjs';

describe('EditStaffComponent', () => {
  let component: EditStaffComponent;
  let fixture: ComponentFixture<EditStaffComponent>;
  let staffService: jasmine.SpyObj<StaffService>;
  let messageService: jasmine.SpyObj<MessageService>;

  beforeEach(async () => {
    const staffServiceSpy = jasmine.createSpyObj('StaffService', ['editStaff']);
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['setMessage']);

    await TestBed.configureTestingModule({
      imports: [RouterTestingModule, FormsModule],
      declarations: [EditStaffComponent],
      providers: [
        { provide: StaffService, useValue: staffServiceSpy },
        { provide: MessageService, useValue: messageServiceSpy },
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(EditStaffComponent);
    component = fixture.componentInstance;
    staffService = TestBed.inject(StaffService) as jasmine.SpyObj<StaffService>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;

    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should call staffService.editStaff on submit', () => {
    component.submitForm = {
      fullName: 'John Doe',
      licenseNumber: '12345',
      phoneNumber: '555-5555',
      email: 'john.doe@example.com',
    };

    component.id = 'D202512344';
    staffService.editStaff.and.returnValue(of({}));

    component.onSubmit({ valid: true });

    expect(staffService.editStaff).toHaveBeenCalled();
  });

  it('should handle error when staffService.editStaff fails', () => {
    component.submitForm = {
      fullName: 'staffMario',
      licenseNumber: '12346',
      phoneNumber: '+951999999998',
      email: 'emaill@gmail.com',
    };

    component.id = 'D202512344';
    staffService.editStaff.and.returnValue(of({ error: { message: 'Error' } }));

    component.onSubmit({ valid: true });

    expect(component.errorMessage).toBe('Error');
  });
});
