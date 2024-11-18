import { ComponentFixture, TestBed } from '@angular/core/testing';
import { EditUserComponent } from './edit-user.component';
import { UserService } from '../../services/user.service';
import { ActivatedRoute, Router } from '@angular/router';
import { MessageService } from '../../services/message.service';
import { of, throwError } from 'rxjs';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';



describe('EditUserComponent', () => {
  let component: EditUserComponent;
  let fixture: ComponentFixture<EditUserComponent>;
  let userService: jasmine.SpyObj<UserService>;
  let messageService: jasmine.SpyObj<MessageService>;
  let router: jasmine.SpyObj<Router>;
  let route: ActivatedRoute;

  beforeEach(async () => {
    const userServiceSpy = jasmine.createSpyObj('UserService', ['edit']);
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['setMessage']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [EditUserComponent, FormsModule, CommonModule],
      providers: [
        { provide: UserService, useValue: userServiceSpy },
        { provide: MessageService, useValue: messageServiceSpy },
        { provide: Router, useValue: routerSpy },
        {
          provide: ActivatedRoute,
          useValue: {
            snapshot: { paramMap: { get: () => 'test@example.com' } }
          }
        }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(EditUserComponent);
    component = fixture.componentInstance;
    userService = TestBed.inject(UserService) as jasmine.SpyObj<UserService>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    route = TestBed.inject(ActivatedRoute);

    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize email from route params', () => {
    expect(component.email).toBe('test@example.com');
  });

  it('should show confirmation modal', () => {
    component.confirmSubmission();
    expect(component.showConfirmation).toBeTrue();
  });

  it('should close confirmation modal', () => {
    component.closeConfirmationModal();
    expect(component.showConfirmation).toBeFalse();
  });

  
  it('should edit user name successfully', () => {
    const userForm = { valid: true };
    component.submitForm = {
      name: 'Test User',
      email: '',
      phoneNumber: ''
    };

    userService.edit.and.returnValue(of({}));

    component.editUser(userForm);

    expect(userService.edit).toHaveBeenCalledWith(
      'test@example.com',
      'Test User',
      '',
      '',
    );

    expect(messageService.setMessage).toHaveBeenCalledWith('User updated successfully');
    expect(router.navigate).toHaveBeenCalledWith(['/profile', 'test@example.com']);
  });

  it('should trigger confirmation to edit user email or phone number successfully', () => {
    const userForm = { valid: true };
    component.submitForm = {
      name: '',
      email: 'new@example.com',
      phoneNumber: '1234567890'
    };

    userService.edit.and.returnValue(of({}));

    component.editUser(userForm);

    expect(userService.edit).toHaveBeenCalledWith(
      'test@example.com',
      '',
      'new@example.com',
      '1234567890',
    );

    expect(component.successMessage).toBe('Please check your email to confirm the changes to your email and/or phone number.');
  });

  it('should handle edit user error', () => {
    const userForm = { valid: true };
    component.submitForm = {
      name: 'Test User',
      email: 'new@example.com',
      phoneNumber: '1234567890'
    };

    userService.edit.and.returnValue(throwError({ error: { message: 'Error' } }));

    component.editUser(userForm);

    expect(component.errorMessage).toBe('An error occurred while updating user: Error');
  });

  it('should navigate to profile on cancel', () => {
    component.onCancel();
    expect(router.navigate).toHaveBeenCalledWith(['/profile', 'test@example.com']);
  });
});
