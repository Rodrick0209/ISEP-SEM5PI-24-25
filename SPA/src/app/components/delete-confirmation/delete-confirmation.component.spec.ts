import { ComponentFixture, TestBed } from '@angular/core/testing';
import { DeleteConfirmationComponent } from './delete-confirmation.component';
import { UserService } from '../../services/user.service';
import { ActivatedRoute, Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { CommonModule } from '@angular/common';

describe('DeleteConfirmationComponent', () => {
  let component: DeleteConfirmationComponent;
  let fixture: ComponentFixture<DeleteConfirmationComponent>;
  let userService: jasmine.SpyObj<UserService>;
  let router: jasmine.SpyObj<Router>;
  let activatedRoute: ActivatedRoute;

  beforeEach(async () => {
    const userServiceSpy = jasmine.createSpyObj('UserService', ['confirmDelete']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [DeleteConfirmationComponent, CommonModule],
      providers: [
        { provide: UserService, useValue: userServiceSpy },
        { provide: Router, useValue: routerSpy },
        {
          provide: ActivatedRoute,
          useValue: {
            queryParams: of({ token: 'test-token', email: 'test@example.com' })
          }
        }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(DeleteConfirmationComponent);
    component = fixture.componentInstance;
    userService = TestBed.inject(UserService) as jasmine.SpyObj<UserService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    activatedRoute = TestBed.inject(ActivatedRoute);
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should call confirmDelete on init with valid token and email', () => {
    userService.confirmDelete.and.returnValue(of({ success: true }));
    spyOn(component, 'confirmDelete').and.callThrough();

    component.ngOnInit();

    expect(component.token).toBe('test-token');
    expect(component.email).toBe('test@example.com');
    expect(component.confirmDelete).toHaveBeenCalled();
  });

  
  it('should navigate to confirmation-success on successful delete', () => {
    userService.confirmDelete.and.returnValue(of({ success: true }));

    component.ngOnInit();
    component.confirmDelete();

    expect(userService.confirmDelete).toHaveBeenCalledWith('test-token', 'test@example.com');
    expect(router.navigate).toHaveBeenCalledWith(['/confirmation-success'], {
      queryParams: { message: 'Your account and all associated data has been deleted succesfully.' }
    });
  });

  it('should navigate to confirmation-error on delete error', () => {
    userService.confirmDelete.and.returnValue(throwError({ error: { message: 'Delete failed' } }));

    component.ngOnInit();
    component.confirmDelete();

    expect(userService.confirmDelete).toHaveBeenCalledWith('test-token', 'test@example.com');
    expect(router.navigate).toHaveBeenCalledWith(['/confirmation-error'], {
      queryParams: { message: 'Delete failed' }
    });
  });

  it('should navigate to confirmation-error with invalid token or email', () => {
    component.token = '';
    component.email = '';

    component.confirmDelete();

    expect(router.navigate).toHaveBeenCalledWith(['/confirmation-error'], {
      queryParams: { message: 'Invalid token or email.' }
    });
  });
});