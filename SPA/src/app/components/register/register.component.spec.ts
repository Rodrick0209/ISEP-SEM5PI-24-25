import { ComponentFixture, TestBed } from '@angular/core/testing';
import { RegisterComponent } from './register.component';
import { UserService } from '../../services/user.service';
import { of, throwError } from 'rxjs';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

describe('RegisterComponent', () => {
  let component: RegisterComponent;
  let fixture: ComponentFixture<RegisterComponent>;
  let userService: jasmine.SpyObj<UserService>;

  beforeEach(async () => {
    const userServiceSpy = jasmine.createSpyObj('UserService', ['register']);

    await TestBed.configureTestingModule({
      imports: [RegisterComponent, FormsModule, CommonModule],
      providers: [{ provide: UserService, useValue: userServiceSpy }]
    }).compileComponents();

    fixture = TestBed.createComponent(RegisterComponent);
    component = fixture.componentInstance;
    userService = TestBed.inject(UserService) as jasmine.SpyObj<UserService>;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should register user successfully', () => {
    const mockResponse = { message: 'Registration successful' };
    userService.register.and.returnValue(of(mockResponse));

    component.name = 'John Doe';
    component.email = 'john.doe@example.com';
    component.phone = '1234567890';
    component.password = 'password123';
    component.onRegister();

    expect(userService.register).toHaveBeenCalledWith('John Doe', 'john.doe@example.com', '1234567890', 'password123');
    expect(component.successMessage).toBe('Registration successful! Please check your email to confirm your account.');
    expect(component.errorMessage).toBeNull();
  });

  it('should handle registration error', () => {
    const mockError = { error: { message: 'Registration failed' } };
    userService.register.and.returnValue(throwError(mockError));

    component.name = 'John Doe';
    component.email = 'john.doe@example.com';
    component.phone = '1234567890';
    component.password = 'password123';
    component.onRegister();

    expect(userService.register).toHaveBeenCalledWith('John Doe', 'john.doe@example.com', '1234567890', 'password123');
    expect(component.errorMessage).toBe('An error occurs when registering the user: Registration failed');
    expect(component.successMessage).toBeNull();
  });
});