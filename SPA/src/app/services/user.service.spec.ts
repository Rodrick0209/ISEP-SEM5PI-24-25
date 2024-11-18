import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { UserService, User } from './user.service';
import { AuthService } from './auth.service';

describe('UserService', () => {
  let service: UserService;
  let httpMock: HttpTestingController;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [UserService, AuthService]
    });
    service = TestBed.inject(UserService);
    httpMock = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should fetch user by email', () => {
    const dummyUser: User = { email: 'test@example.com' };

    service.getUserByEmail('test@example.com').subscribe(user => {
      expect(user).toEqual(dummyUser);
    });

    const req = httpMock.expectOne('/api/users/email/test@example.com');
    expect(req.request.method).toBe('GET');
    req.flush(dummyUser);
  });

  it('should register a new user', () => {
    const dummyResponse = { success: true };

    service.register('John Doe', 'john@example.com', '1234567890', 'password123').subscribe(response => {
      expect(response).toEqual(dummyResponse);
    });

    const req = httpMock.expectOne('/api/users/patients');
    expect(req.request.method).toBe('POST');
    expect(req.request.body).toEqual({
      name: 'John Doe',
      email: 'john@example.com',
      phoneNumber: '1234567890',
      password: 'password123'
    });
    req.flush(dummyResponse);
  });

  it('should confirm registration', () => {
    const dummyResponse = { success: true };

    service.confirmRegistration('token123', 'john@example.com').subscribe(response => {
      expect(response).toEqual(dummyResponse);
    });

    const req = httpMock.expectOne('/api/users/patients/confirm?token=token123&email=john@example.com');
    expect(req.request.method).toBe('POST');
    req.flush(dummyResponse);
  });

  it('should edit user details', () => {
    const dummyResponse = { success: true };

    service.edit('john@example.com', 'John Doe', 'john.new@example.com', '0987654321').subscribe(response => {
      expect(response).toEqual(dummyResponse);
    });

    const req = httpMock.expectOne('/api/users/patients/edit');
    expect(req.request.method).toBe('PATCH');
    expect(req.request.body).toEqual({
      email: 'john@example.com',
      nameToEdit: 'John Doe',
      emailToEdit: 'john.new@example.com',
      phoneNumberToEdit: '0987654321'
    });
    req.flush(dummyResponse);
  });

  it('should confirm edit', () => {
    const dummyResponse = { success: true };

    service.confirmEdit('token123', 'john@example.com', 'john.new@example.com', '0987654321').subscribe(response => {
      expect(response).toEqual(dummyResponse);
    });

    const req = httpMock.expectOne('/api/users/patients/edit/confirm?token=token123&email=john@example.com&emailToEdit=john.new@example.com&phoneNumberToEdit=0987654321');
    expect(req.request.method).toBe('PATCH');
    req.flush(dummyResponse);
  });

  it('should send forgot password request', () => {
    const dummyResponse = { success: true };

    service.forgotPassword('john@example.com').subscribe(response => {
      expect(response).toEqual(dummyResponse);
    });

    const req = httpMock.expectOne('/api/Users/Forgot-Password');
    expect(req.request.method).toBe('POST');
    expect(req.request.body).toEqual({ email: 'john@example.com' });
    req.flush(dummyResponse);
  });

  it('should reset password', () => {
    const dummyResponse = { success: true };

    service.resetPassword('token123', 'newPassword123', 'john@example.com').subscribe(response => {
      expect(response).toEqual(dummyResponse);
    });

    const req = httpMock.expectOne('/api/Users/Reset-Password');
    expect(req.request.method).toBe('POST');
    expect(req.request.body).toEqual({
      token: 'token123',
      newPassword: 'newPassword123',
      email: 'john@example.com'
    });
    req.flush(dummyResponse);
  });

  it('should delete user', () => {
    const dummyResponse = { success: true };

    service.delete('test@example.com').subscribe(response => {
      expect(response).toEqual(dummyResponse);
    });

    const req = httpMock.expectOne('/api/users/patients/delete/test@example.com');
    expect(req.request.method).toBe('DELETE');

    req.flush(dummyResponse);
  });

  it('should confirm user deletion', () => {
    const dummyResponse = { success: true };

    service.confirmDelete('token123', 'test@example.com').subscribe(response => {
      expect(response).toEqual(dummyResponse);
    });

    const req = httpMock.expectOne('/api/users/patients/delete/confirm?token=token123&email=test@example.com');
    expect(req.request.method).toBe('DELETE');

    req.flush(dummyResponse);
  });
});