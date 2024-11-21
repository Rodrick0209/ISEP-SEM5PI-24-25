import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute } from '@angular/router';
import { of } from 'rxjs';
import { ConfirmationSuccessComponent } from './confirmation-success.component';


describe('ConfirmationSuccessComponent', () => {
  let component: ConfirmationSuccessComponent;
  let fixture: ComponentFixture<ConfirmationSuccessComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ConfirmationSuccessComponent],
      providers: [
        {
          provide: ActivatedRoute,
          useValue: {
            queryParams: of({ message: 'Confirmation successful!' })
          }
        }
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    TestBed.overrideProvider(ActivatedRoute, {
      useValue: {
        queryParams: of({})
      }
    });
    fixture = TestBed.createComponent(ConfirmationSuccessComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should set default successMessage if no message in queryParams', () => {
    expect(component.successMessage).toBe('An error occurred during confirmation.');
  });
});